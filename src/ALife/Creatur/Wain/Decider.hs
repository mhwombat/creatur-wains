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
    randomDecider,
    somOK
  ) where

import ALife.Creatur.Wain.GeneticSOM (GeneticSOM, Label, patternMap,
  justClassify, reportAndTrain, randomGeneticSOM, somOK, numModels, toList)
import ALife.Creatur.Wain.Response (Response, copyOutcomeTo,
  randomResponse)
import Control.Monad (replicateM)
import Control.Monad.Random (Rand, RandomGen)
import Data.Word (Word8)
import Math.Geometry.GridMap ((!))
import System.Random (Random)

type Decider a = GeneticSOM (Response a)

randomDecider
  :: (RandomGen g, Eq a, Random a)
    => Word8 -> Word8 -> Rand g (Decider a)
randomDecider numClassifierModels deciderSize = do
  let k = numTiles $ fromIntegral deciderSize
  xs <- replicateM k (randomResponse $ fromIntegral numClassifierModels)
  randomGeneticSOM deciderSize xs

numTiles :: Int -> Int
numTiles s = 3*s*(s-1) + 1

predict :: (Eq a) => Decider a -> Response a -> Response a
predict d r = a `copyOutcomeTo` r
  where k = justClassify d r
        a = patternMap d ! k

feedback :: (Eq a) => Decider a -> Response a -> Decider a
feedback d r = d'
  where (_, _, d') = reportAndTrain d r
