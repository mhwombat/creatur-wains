------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Decider
-- Copyright   :  (c) Amy de Buitléir 2013
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
    numModels,
    toList,
    possibleResponses,
    possibleActions, -- exported for testing
    -- randomDecider,
    somOK
  ) where

import ALife.Creatur.Wain.GeneticSOM (GeneticSOM, Label, patternMap,
  justClassify, reportAndTrain, somOK, numModels, toList, models)
import ALife.Creatur.Wain.Response (Response(..), action,
  diffIgnoringOutcome)
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
predict :: (Eq a) => Decider a -> Response a -> (Response a, Label)
predict d r = (r { outcome=adjustedOutcome }, k)
  where k = justClassify d r
        a = patternMap d ! k
        rawOutcome = fromJust . outcome $ a
        adjustedOutcome = Just $ (1 - diffIgnoringOutcome a r)*rawOutcome

-- | Updates the decider models based on the outcome of the response
--   provided. The response should contain an outcome value.
feedback :: (Eq a) => Decider a -> Response a -> Decider a
feedback d r = d'
  where (_, _, d') = reportAndTrain d r

-- | Returns the list of possible responses to the given scenario.
--   The outcome field in each response will be @Nothing@;
--   use @predict@ to calculate the outcome.
possibleResponses :: (Eq a) => Decider a -> Scenario -> [Response a]
possibleResponses d s
  = map (\a -> Response s a Nothing) $ possibleActions d

-- | Returns the list of actions that this decider "knows".
possibleActions :: (Eq a) => Decider a -> [a]
possibleActions = nub . map action . models 
