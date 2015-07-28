------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Predictor
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A prediction model based on a Kohonen Self-organising Map (SOM).
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module ALife.Creatur.Wain.Predictor
  (
    Label,
    Predictor,
    PredictorTweaker(..),
    buildPredictor,
    predict,
    imprint,
    predictorQuality
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (GeneticSOM, ExponentialParams(..),
  Label, Tweaker(..), buildGeneticSOM, tweaker, discrimination,
  classify, train, modelMap, counterMap)
import ALife.Creatur.Wain.Response (Response(..), outcome,
  diffIgnoringOutcome, similarityIgnoringOutcome, makeResponseSimilar,
  action)
import ALife.Creatur.Wain.Scenario (Scenario)
import ALife.Creatur.Wain.PlusMinusOne (adjustPM1Double)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.Weights (Weights)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Function (on)
import Data.List (groupBy, sortBy)
import qualified Data.Map.Strict as M
import Data.Serialize (Serialize)
import Data.Word (Word16)
import GHC.Generics (Generic)

-- | @'PredictorTweaker' cw rw@ constructs an object which is
--   responsible for comparing and adjusting response patterns.
--   The parameter @cw@ determines the relative weight to assign to
--   differences in energy, passion, and whether or not there is a
--   litter when comparing two patterns.
--   The parameter @rw@ determines the relative weight to assign to
--   differences in the scenarios and the outcomes when comparing two
--   patterns.
--
--   NOTE: In order to declare an instance of @Tweaker@, we have to
--   reference the action type @a@. As a result, @PredictorTweaker@ has
--   to have a type parameter @a@, even though it is not used.
data PredictorTweaker a = PredictorTweaker Weights Weights
  deriving (Eq, Show, Generic, NFData, Serialize, Genetic, Diploid)

instance (Eq a) => Tweaker (PredictorTweaker a) where
  type Pattern (PredictorTweaker a) = Response a
  diff (PredictorTweaker cw rw) = diffIgnoringOutcome cw rw
  adjust _ = makeResponseSimilar

type Predictor a = GeneticSOM (Response a) (PredictorTweaker a)

-- | @'buildPredictor' e n dt cw sw rw@ returns a Predictor, using an
--   exponential function with the parameters @e@ as a learning
--   function, maximum number of models @n@, difference threshold @dt@,
--   and "tweaker" defined by the weights @cw@, @sw@, and @rw@.
buildPredictor
  :: Eq a
    => ExponentialParams -> Word16 -> UIDouble -> Weights -> Weights
      -> Predictor a
buildPredictor e n dt cw rw
  = buildGeneticSOM e n dt (PredictorTweaker cw rw)

-- | Predicts the outcome of a response based on the predictor models,
--   and updates the outcome field in that response.
--   If the Predictor has no model for this response, it creates a new
--   model and returns the response unmodified.
predict
  :: (Eq a) => Predictor a -> (Response a, UIDouble)
    -> (Response a, Label, Predictor a)
predict d (r, k) = (r', bmu, d')
  where (bmu, _, _, d') = classify d r
        model = modelMap d' M.! bmu
        rawOutcome = _outcome model
        (PredictorTweaker cw rw)  = view tweaker d
        adjustment = k * (similarityIgnoringOutcome cw rw model r)
        -- Adjust from zero because, depending on the similarity
        -- between the scenario and the model, the action may have less
        -- of an effect than predicted by the model.
        adjustedOutcome
          = adjustPM1Double rawOutcome adjustment 0
        r' = set outcome adjustedOutcome r

-- | Teaches a response to the Predictor (teaches it that the response
--   increases happiness by 1).
imprint :: (Eq a) => Predictor a -> Scenario -> a -> Predictor a
imprint d s a = d'
  where r = Response s a 1
        d' = train d r

predictorQuality :: (Eq a, Ord a) => Predictor a -> Int
predictorQuality d = discrimination actionCounts
  where modelCounts = M.elems . counterMap $ d
        actions = map (view action) . M.elems . modelMap $ d
        actionCounts = map (sum . map snd)
                         . groupBy ((==) `on` fst)
                         . sortBy (compare `on` fst)
                           $ zip actions modelCounts
