------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Classifier
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A classifier based on a Kohonen Self-organising Map (SOM).
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ALife.Creatur.Wain.Classifier
  (
    Label,
    Classifier,
    buildClassifier,
    classify,
    classifierOK
  ) where

import ALife.Creatur.Wain.GeneticSOM (GeneticSOM,
  ExponentialParams(..), Thinker(..), Label, buildGeneticSOM,
  reportAndTrain, somOK)

type Classifier = GeneticSOM

buildClassifier
  :: (Thinker t, p ~ Pattern t)
    => ExponentialParams -> t -> [p] -> Classifier p t
buildClassifier = buildGeneticSOM

-- | Returns @True@ if the SOM has a valid Exponential and at least one
--   model; returns @False@ otherwise.
classifierOK
  :: Classifier s t -> Bool
classifierOK = somOK

-- | Updates the classifier models based on the stimulus (input).
--   Returns the index (grid location) of the model that most closely
--   matches the input pattern, the differences between the input
--   pattern and each model in the classifier, and the updated
--   classifier (the counter for the closest model is incremented).
classify
  :: Classifier s t -> s -> ([Double], Classifier s t)
classify c p = (sig, c')
  where (_, diffs, c') = reportAndTrain c p
        sig = map snd diffs

-- conflation :: Metric s ~ Double => Classifier s -> Double
-- conflation c = conflation' $ counts c

-- conflation' :: [Word16] -> Double
-- conflation' [] = 0
-- conflation' (_:[]) = 0
-- conflation' xs =
--   if numVotes == 0
--      then 0
--      else chiSquared / chiSquaredMax
--   where chiSquared = sum $ map (\x -> (x - expected)*(x - expected)/expected) xs'
--         chiSquaredMax = fromIntegral numVotes * fromIntegral (length xs - 1)
--         expected = fromIntegral numVotes / fromIntegral (length xs)
--         numVotes = sum $ map fromIntegral xs :: Int
--         xs' = map fromIntegral xs

-- discrimination
--   :: (Pattern s, Metric s ~ Double)
--     => Classifier s -> Int -> Double
-- discrimination c maxCategories = (kMax - k) / kMax
--   where kMax = fromIntegral maxCategories
--         k = fromIntegral (numModels c)
