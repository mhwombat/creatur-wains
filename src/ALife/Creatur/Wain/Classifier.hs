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
    Signature,
    Classifier,
    buildClassifier,
    classifySetAndTrain
  ) where

import ALife.Creatur.Wain.GeneticSOM (GeneticSOM, Signature,
  ExponentialParams(..), Tweaker(..), Label, buildGeneticSOM,
  classify, train)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Data.List (foldl')
import Data.Word (Word16)

type Classifier = GeneticSOM

-- | @'buildClassifier' p n dt t@ returns a genetic SOM, using an
--   exponential function with the parameters @p@ as a learning
--   function, maximum number of models @n@, difference threshold @dt@,
--   and "tweaker" @t@.
buildClassifier
  :: (Tweaker t, p ~ Pattern t)
    => ExponentialParams -> Word16 -> UIDouble -> t -> Classifier p t
buildClassifier = buildGeneticSOM

-- | Updates the classifier models based on the stimulus (set of
--   input patterns).
--   Returns the "signature" (differences between the input pattern
--   and each model in the classifier) of each input pattern,
--   and the updated classifier.
classifySetAndTrain
  :: Classifier s t -> [s] -> ([Label], [Signature], Classifier s t)
classifySetAndTrain c ps = (reverse bmus, reverse ds, c')
  where (bmus, ds, c') = foldl' classifyNextAndTrain ([], [], c) ps

classifyNextAndTrain
  :: ([Label], [Signature], Classifier s t)
    -> s -> ([Label], [Signature], Classifier s t)
classifyNextAndTrain (bmus, ds, c) p = (bmu:bmus, d:ds, c')
  where (bmu, d, c') = classifyAndTrain c p

-- | Updates the classifier models based on the input pattern.
--   Returns the "signature" (differences between the input pattern
--   and each model in the classifier) and the updated classifier.
classifyAndTrain
  :: Classifier s t -> s
    -> (Label, Signature, Classifier s t)
classifyAndTrain c p = (bmu, diffs, c3)
  where (bmu, _, diffs, c2) = classify c p
        c3 = train c2 p
