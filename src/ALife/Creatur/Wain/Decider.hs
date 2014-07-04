------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Decider
-- Copyright   :  (c) Amy de Buitléir 2013-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A decision-maker based on a Kohonen Self-organising Map (SOM).
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module ALife.Creatur.Wain.Decider
  (
    Label,
    Decider,
    predict,
    feedback,
    justClassify,
    numModels,
    toList,
    possibleActions, -- exported for testing
    -- randomDecider,
    somOK
  ) where

import ALife.Creatur.Wain.GeneticSOM (GeneticSOM, Label, patternMap,
  justClassify, reportAndTrain, somOK, numModels, toList, models)
import ALife.Creatur.Wain.Response (Response(..), action,
  similarityIgnoringOutcome, setOutcome)
import ALife.Creatur.Wain.Scenario (Scenario)
import Data.List (nub)
import Data.Maybe (fromJust)
import Math.Geometry.GridMap ((!))

type Decider a = GeneticSOM (Response a)

-- randomDecider
--   :: (RandomGen g, Eq a, Random a)
--     => Word8 -> Word8 -> Rand g (Decider a)
-- randomDecider numClassifierModels deciderSize = do
--   let k = numTiles $ fromIntegral deciderSize
--   xs <- replicateM k (randomResponse $ fromIntegral numClassifierModels)
--   randomGeneticSOM deciderSize xs

-- | Predicts the outcome of a response based on the decider models,
--   and updates the outcome field in that response.
predict
  :: (Eq a)
    => Decider a -> Scenario -> a -> (Response a, Label)
predict d s a = (r3, k)
  where r = Response s a Nothing
        k = justClassify d r
        model = patternMap d ! k
        rawOutcome = fromJust . outcome $ model
        adjustedOutcome
          = Just $ (similarityIgnoringOutcome model r)*rawOutcome
        r3 = r { outcome=adjustedOutcome }

-- | Updates the decider models based on the outcome of the response
--   provided. The response should contain an outcome value.
feedback :: (Eq a) => Decider a -> Response a -> Label -> Double -> Decider a
feedback d r _ o = d'
  where (_, _, d') = reportAndTrain d r'
        r' = r `setOutcome` o
        -- model = patternMap d ! k
        -- rawOutcome = o / (similarityIgnoringOutcome model r)
        -- newModel = model `setOutcome` rawOutcome

-- | Returns the list of actions that this decider "knows".
possibleActions :: (Eq a) => Decider a -> [a]
possibleActions = nub . map action . models 
