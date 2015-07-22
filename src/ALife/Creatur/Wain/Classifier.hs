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
    classifySetAndTrain
  ) where

import ALife.Creatur.Wain.GeneticSOM (GeneticSOM,
  ExponentialParams(..), Tweaker(..), Label, buildGeneticSOM,
  classify, train)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Data.List (foldl')

type Classifier = GeneticSOM

-- | @'buildClassifier' p n dt t@ returns a genetic SOM, using an
--   exponential function with the parameters @p@ as a learning
--   function, maximum number of models @n@, difference threshold @dt@,
--   and "tweaker" @t@.
buildClassifier
  :: (Tweaker t, p ~ Pattern t)
    => ExponentialParams -> Int -> UIDouble -> t -> Classifier p t
buildClassifier = buildGeneticSOM

-- | Updates the classifier models based on the stimulus (set of
--   input patterns).
--   Returns the IDs of the models that most closely match each input
--   pattern,
--   the "novelty" of each input pattern (difference between it and
--   the best matching model),
--   the "signature" (differences between the input pattern
--   and each model in the classifier), and the updated classifier.
classifySetAndTrain
  :: Classifier s t -> [s]
    -> ([Label], [UIDouble], [[UIDouble]], Classifier s t)
classifySetAndTrain c ps = (reverse ks, reverse ns, reverse ds, c')
  where (ks, ns, ds, c') = foldl' classifyNextAndTrain ([], [], [], c) ps

classifyNextAndTrain
  :: ([Label], [UIDouble], [[UIDouble]], Classifier s t)
    -> s -> ([Label], [UIDouble], [[UIDouble]], Classifier s t)
classifyNextAndTrain (ks, ns, ds, c) p = (k:ks, n:ns, d:ds, c')
  where (k, n, d, c') = classifyAndTrain c p

-- | Updates the classifier models based on the input pattern.
--   Returns the ID of the model that most closely matches the pattern,
--   the "novelty" of the the pattern @p@ (difference between it and
--   the best matching model),
--   the "signature" (differences between the input pattern
--   and each model in the classifier), and the updated classifier.
classifyAndTrain
  :: Classifier s t -> s -> (Label, UIDouble, [UIDouble], Classifier s t)
classifyAndTrain c p = (bmu, nov, sig, c3)
  where (bmu, nov, diffs, c2) = classify c p
        c3 = train c2 p
        sig = map snd diffs
