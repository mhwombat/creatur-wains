------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Classifier
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A classifier based on a Kohonen Self-organising Map (SOM).
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
module ALife.Creatur.Wain.Classifier
  (
    Classifier,
    classify,
    numModels,
    models,
    conflation,
    conflation', -- exported for testing
    randomClassifier
  ) where

import ALife.Creatur.Wain.GeneticSOM (GeneticSOM(..), Label,
  models, reportAndTrain, numModels, counts, randomGeneticSOM)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Monad.Random (Rand, RandomGen)
import Data.Datamining.Pattern (Pattern, Metric)
import Data.Word (Word8)

type Classifier = GeneticSOM

randomClassifier
  :: (Pattern p, Metric p ~ UIDouble, RandomGen g)
    => Word8 -> [p] -> Rand g (Classifier p)
randomClassifier = randomGeneticSOM

-- | Find out how similar the stimulus (input) is to the current
--   classifier models.
classify :: (Pattern s, Ord (Metric s)) => Classifier s -> s -> (Label, [Metric s], Classifier s)
classify c p = (bmu, sig, c')
  where (bmu, diffs, c') = reportAndTrain c p
        sig = map snd diffs

conflation :: Classifier s -> Double
conflation c = conflation' . counts $ c

conflation' :: [Int] -> Double
conflation' [] = 0
conflation' (_:[]) = 0
conflation' xs =
  if numPatterns == 0
     then 0
     else chiSquared / chiSquaredMax
  where chiSquared = sum . map (\x -> (x - expected)*(x - expected)/expected) $ xs'
        chiSquaredMax = fromIntegral $ numPatterns * (length xs - 1)
        expected = fromIntegral numPatterns / fromIntegral (length xs)
        numPatterns = sum xs
        xs' = map fromIntegral xs
