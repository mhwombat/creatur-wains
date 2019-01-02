------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PredictorInternal
-- Copyright   :  (c) Amy de Buitléir 2013-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private Predictor internals.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ALife.Creatur.Wain.PredictorInternal where

import qualified ALife.Creatur.Wain.Classifier   as Cl
import qualified ALife.Creatur.Wain.GeneticSOM   as S
import           ALife.Creatur.Wain.PlusMinusOne
    (PM1Double, adjustPM1Vector, pm1ToDouble)
import           ALife.Creatur.Wain.Pretty
    (Pretty (..))
import           ALife.Creatur.Wain.Probability
    (Probability, prettyProbability)
import           ALife.Creatur.Wain.Response
    (Response (..), addToOutcomes, outcomes)
import           ALife.Creatur.Wain.UnitInterval
    (UIDouble)
import           Control.DeepSeq
    (NFData)
import           Control.Lens
import           Data.List
    (intercalate, nub)
import qualified Data.Map.Strict                 as M
import           Data.Word
    (Word64)
import           GHC.Generics
    (Generic)
import           Text.Printf
    (printf)

-- | A predictor predicts the outcome of a response to a scenario.
type Predictor a t = S.GeneticSOM (Response a) t

-- | @'buildPredictor' e n dt t@ returns a Predictor, using an
--   learning function with the parameters @e@ as a learning
--   function, maximum number of models @n@,
--   difference threshold @dt@, and tweaker @t@.
buildPredictor
  :: (Eq a, S.Tweaker t, S.Pattern t ~ Response a)
    => S.LearningParams -> Word64 -> UIDouble -> t -> Predictor a t
buildPredictor = S.buildGeneticSOM

-- | Information about how a predictor generated a prediction
data PredictionDetail a
  = PredictionDetail
      {
        -- | The response, updated with the predicted outcomes
        pResponse    :: Response a,
        -- | The probability of the scenario on which the prediction
        --   is based
        pProb        :: Probability,
        -- | The label of the node that best matches the input
        pBmu         :: S.Label,
        -- | The BMU's model
        pBmuModel    :: Response a,
        -- | A measure of how novel the response pattern was to the wain.
        --   It is the difference between the input pattern and the
        --   closest model prior to any training or addition of models.
        pNovelty     :: S.Difference,
        -- | A measure of how novel the response pattern was to the wain,
        --   adjusted based on the age of the wain.
        pAdjNovelty  :: Int,
        -- | The adjusted probability based on how well the model
        --   matches the proposed response
        pAdjustment  :: Probability,
        -- | The unadjusted outcomes from the model
        pRawOutcomes :: [PM1Double],
        -- | Even more details about the prediction
        pDetails     :: M.Map S.Label (Response a, S.Difference)
      } deriving (Generic, Show, NFData)

instance (Pretty a) => Pretty (PredictionDetail a) where
  pretty r = pretty (pResponse r)
               ++ " prob: " ++ prettyProbability (pProb r)
               ++ " based on model " ++ pretty (pBmu r)
               ++ " raw outcomes: "
               ++ intercalate " " (map (printf "%.3f" . pm1ToDouble) os)
    where os = pRawOutcomes r

-- | @'predict' p r k@ uses the predictor @p@ to predict the outcome
--   of the response @r@ to the onput @p@, given the probability @k@
--   that the scenario associated with the response is the one
--   we're actually facing.
--   Returns a detailed report including the prediction and
--   information about how the prediction was generated.
predict
  :: (Eq a, S.Tweaker t, S.Pattern t ~ Response a)
    => Predictor a t -> Response a -> Probability
    -> PredictionDetail a
predict p r prob = PredictionDetail
                     {
                       pResponse = r',
                       pProb = prob,
                       pBmu = bmu,
                       pBmuModel = model,
                       pNovelty = novelty,
                       pAdjNovelty = S.cAdjNovelty report,
                       pAdjustment = adjustment,
                       pRawOutcomes = rawOutcomes,
                       pDetails = S.cDetails report
                     }
  where (report, _) = S.trainAndClassify p r
        bmu = S.cBmu report
        novelty = S.cNovelty report
        -- If the predictor already contained a suitable model,
        -- then it was trained with r (which contains default outcomes);
        -- the trained model (S.cBmuModel report) is not useful.
        model = if fromIntegral bmu < S.numModels p
                  then p `S.modelAt` bmu
                  else r
        rawOutcomes = view outcomes model
        -- Adjust the outcome based on how well the model
        -- matches the proposed response. Specifically, we're comparing
        -- the classifications and the conditions, in the model and the
        -- proposed response.
        adjustment = prob * (1 - novelty)
        -- Adjust from zero because, depending on the similarity
        -- between the true scenario and the model, the action may have
        -- less of an effect (positive or negative) than predicted by
        -- the model.
        zeroes = map (const 0) rawOutcomes
        adjustedOutcomes
          = adjustPM1Vector rawOutcomes adjustment zeroes
        r' = set outcomes adjustedOutcomes r

-- | Information about how a predictor learned.
data LearningReport p a
  = LearningReport
      {
        -- | The current learning rate for the predictor
        lLearningRate :: UIDouble,
        -- | Is the pattern new (imprinted) or old (reinforced)
        lImprinted    :: Bool,
        -- | The response that was learned
        lResponse     :: p,
        -- | The label of the predictor node that best matches the input
        lBmu          :: S.Label,
        -- | A measure of how novel the input pattern was to the wain.
        lNovelty      :: S.Difference,
        -- | A measure of how novel the input pattern was to the wain,
        --   adjusted based on the age of the wain.
        lAdjNovelty   :: Int,
        -- | Even more details about the classification
        lDetails      :: M.Map S.Label (p, S.Difference)
      } deriving (Generic, Show, NFData)

prettyLearningReport
  :: Pretty p
  => LearningReport p a -> [String]
prettyLearningReport r =
  [
    "predictor learning rate: " ++ pretty (lLearningRate r),
    msg ++ pretty (lResponse r),
    " predictor BMU: " ++ pretty (lBmu r)
      ++ " difference: " ++ pretty (lNovelty r)
      ++ " novelty: " ++ pretty (lAdjNovelty r),
    "  learning details (label, model, diff):"
  ] ++ S.prettyClassificationMoreDetail (lDetails r)
  where msg = if lImprinted r
                then "Imprinted new response model. "
                else "Reinforced existing response model. "

-- | @'imprintOrReinforce' d ls a os deltas@ teaches the predictor that
--   the action @a@ is a good response when facing the scenario @ls@.
--   If this response is new to the predictor, it will store it as a new
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
    => Predictor a t -> [S.Label] -> a -> [PM1Double] -> [PM1Double]
      -> (LearningReport (Response a) a, Predictor a t)
imprintOrReinforce d ls a os deltas =
  if existingModel
    then (reportR, dR)
    else (reportI, dI)
  where (reportI, dI) = learn d rI -- imprinting new model
        (reportR, dR) = learn d rR -- reinforcing existing model
        r = (S.modelMap d) M.! bmuI
        rI = Response ls a os
        rR = deltas `addToOutcomes` r
        bmuI = lBmu reportI
        existingModel = d `S.hasLabel` bmuI

-- | @'learn' p r@ teaches the response @r@ to the predictor @p@.
learn
  :: (Eq a)
  => Predictor a t -> Response a
  -> (LearningReport (Response a) a, Predictor a t)
learn d r = (report', d')
  where (report, d') = S.trainAndClassify d r
        bmu = S.cBmu report
        existing = d `S.hasLabel` bmu
        report' = LearningReport
                    {
                      lLearningRate = S.currentLearningRate d,
                      lImprinted = not existing,
                      lResponse = r,
                      lBmu = S.cBmu report,
                      lNovelty = S.cNovelty report,
                      lAdjNovelty = S.cAdjNovelty report,
                      lDetails = S.cDetails report
                    }

-- | Returns the set of scenarios for which this predictor has response
--   models.
scenarios :: Predictor a t -> [[Cl.Label]]
scenarios = map (_labels . snd) . M.toList . S.modelMap

-- | Returns @True@ if the predictor has a response for the scenario;
--   returns @False@ otherwise.
hasScenario :: Predictor a t -> [Cl.Label] -> Bool
hasScenario p ls = ls `elem` (scenarios p)

actions :: Eq a => Predictor a t -> [a]
actions = nub . map (_action . snd) . M.toList . S.modelMap

