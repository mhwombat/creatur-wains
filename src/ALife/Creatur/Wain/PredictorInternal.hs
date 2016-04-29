------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PredictorInternal
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private Predictor internals.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ALife.Creatur.Wain.PredictorInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (GeneticSOM, LearningParams,
  Label, Tweaker(..), buildGeneticSOM, modelMap, trainAndClassify,
  hasLabel, train)
import qualified ALife.Creatur.Wain.Classifier as Cl
import ALife.Creatur.Wain.Response (Response(..), outcomes,
  responseDiff, makeResponseSimilar, addToOutcomes)
import ALife.Creatur.Wain.Probability (Probability)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, adjustPM1Vector)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.DeepSeq (NFData)
import Control.Lens
import qualified Data.Map.Strict as M
import Data.Serialize (Serialize)
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | @'PredictorTweaker'@ constructs an object which is
--   responsible for comparing and adjusting response patterns.
--
--   NOTE: In order to declare an instance of @Tweaker@, we have to
--   reference the action type @a@. As a result, @PredictorTweaker@ has
--   to have a type parameter @a@, even though it is not used.
data PredictorTweaker a = PredictorTweaker
  deriving (Eq, Show, Generic, NFData, Serialize, Genetic, Diploid)

instance (Eq a) => Tweaker (PredictorTweaker a) where
  type Pattern (PredictorTweaker a) = Response a
  diff _ = responseDiff
  adjust _ = makeResponseSimilar

-- | A predictor predicts the outcome of a response to a scenario.
type Predictor a = GeneticSOM (Response a) (PredictorTweaker a)

-- | @'buildPredictor' e n dt@ returns a Predictor, using an
--   learning function with the parameters @e@ as a learning
--   function, maximum number of models @n@,
--   and difference threshold @dt@.
buildPredictor
  :: Eq a
    => LearningParams -> Word64 -> UIDouble -> Predictor a
buildPredictor e n dt = buildGeneticSOM e n dt PredictorTweaker

-- | @'predict' p r k@ uses the predictor @p@ to predict the outcome
--   of the response @r@, given the probability @k@ that the scenario
--   associated with the response is the one we're actually facing.
--   Returns the updated response (with the predicted outcome filled
--   in), the label of the predictor model that best matches the
--   response, and the (possibly updated) predictor.
--   If the Predictor has no model for this response, it creates a new
--   predictor model and returns the response unmodified.
predict
  :: (Eq a) => Predictor a -> Response a -> Probability
    -> (Response a, Label, [PM1Double], Predictor a)
predict p r prob = (r', bmu, rawOutcomes, p')
  where (bmu, p') = classifyAndMaybeCreateNewModel p r
        model = modelMap p' M.! bmu
        rawOutcomes = view outcomes model
        -- Adjust the outcome based on how well the model
        -- matches the proposed response. Specifically, we're comparing
        -- the classifications and the conditions, in the model and the
        -- proposed response.
        adjustment = prob * (1 - responseDiff model r)
        -- Adjust from zero because, depending on the similarity
        -- between the true scenario and the model, the action may have
        -- less of an effect (positive or negative) than predicted by
        -- the model.
        zeroes = map (const 0) rawOutcomes
        adjustedOutcomes
          = adjustPM1Vector rawOutcomes adjustment zeroes
        r' = set outcomes adjustedOutcomes r

-- | @'imprintOrReinforce' d ls a os deltas@ teaches the predictor that
--   the action @a@ is a good response when facing the scenario @ls@.
--   If this response is new to the predictor, it will store it as a
--   model with outcomes @os@. If the response is known, the predictor
--   will learn (reinforce) the model with @deltas@ added to its
--   outcomes.
--   Returns the label of the new or adjusted response model,
--   the new or adjusted response model itself,
--   and the updated predictor.
--   Note: The current learning rate applies when reinforcing a model,
--   so do not expect the new model to have new outcomes = previous
--   outcomes + @deltas@.
imprintOrReinforce
  :: (Eq a)
    => Predictor a -> [Label] -> a -> [PM1Double] -> [PM1Double]
      -> (Label, Response a, Predictor a)
imprintOrReinforce d ls a os deltas =
  if d `hasLabel` bmu
    then (bmu, rReinforced, dReinforced)
    else (bmu, rImprinted, dImprinted)
  where (bmu, dImprinted) = classifyAndMaybeCreateNewModel d rImprinted
        r = (modelMap d) M.! bmu
        rImprinted = Response ls a os
        rReinforced = deltas `addToOutcomes` r
        dReinforced = train d rReinforced

-- | Don't modify existing models, but do permit a new one to be
--   created.
classifyAndMaybeCreateNewModel
  :: (Eq a) => Predictor a -> Response a -> (Label, Predictor a)
classifyAndMaybeCreateNewModel p r =
  if bmu `elem` (M.keys . modelMap $ p)
    then (bmu, p)
    else (bmu, p')
  where (bmu, _, _, p') = trainAndClassify p r

-- | Returns the set of scenarios for which this predictor has response
--   models.
scenarios :: Predictor a -> [[Cl.Label]]
scenarios = map (_labels . snd) . M.toList . modelMap

-- | Returns @True@ if the predictor has a response for the scenario;
--   returns @False@ otherwise.
hasScenario :: Predictor a -> [Cl.Label] -> Bool
hasScenario p ls = ls `elem` (scenarios p)
