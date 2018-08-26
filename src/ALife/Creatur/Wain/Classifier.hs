------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Classifier
-- Copyright   :  (c) Amy de Buitléir 2012-2018
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
    S.Label,
    S.Difference,
    Classifier,
    buildClassifier,
    classifySetAndTrain
  ) where

import qualified ALife.Creatur.Wain.GeneticSOM as S
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Data.List (foldl')
import Data.Word (Word64)

-- | Maintains a set of models to represent the input patterns
--   encountered, and classifies the input patterns according to the
--   models.
type Classifier = S.GeneticSOM

-- | @'buildClassifier' p n dt t@ returns a genetic SOM, using an
--   learning function with the parameters @p@ as a learning
--   function, maximum number of models @n@, difference threshold @dt@,
--   and "tweaker" @t@.
buildClassifier
  :: (S.Tweaker t, p ~ S.Pattern t)
    => S.LearningParams -> Word64 -> UIDouble -> t -> Classifier p t
buildClassifier = S.buildGeneticSOM

-- | Updates the classifier models based on the stimulus (set of
--   input patterns).
--   Returns the labels of the (possibly new) models that are closest
--   to each input pattern,
--   the SGM labels paired with the difference between the
--   inputs and the corresponding model,
--   and the updated classifier.
classifySetAndTrain
  :: Classifier s t -> [s]
    -> ([S.Label], [[(S.Label, S.Difference)]], Classifier s t)
classifySetAndTrain c ps
  = (reverse bmus, reverse diffs, c')
  where (bmus, diffs, c')
          = foldl' classifyNextAndTrain ([], [], c) ps

classifyNextAndTrain
  :: ([S.Label], [[(S.Label, S.Difference)]], Classifier s t)
    -> s
    -> ([S.Label], [[(S.Label, S.Difference)]], Classifier s t)
classifyNextAndTrain (bmus, diffs, c) p
  = (bmu:bmus, d:diffs, c')
  where (bmu, d, c') = classifyAndTrain c p

-- | Updates the classifier models based on the input pattern.
--   Returns the label of the closest (possibly new) model,
--   the SGM labels paired  with the difference between the
--   input and the corresponding model,
--   the label of the closest already existing model,
--   and the updated classifier.
classifyAndTrain
  :: Classifier s t -> s
    -> (S.Label, [(S.Label, S.Difference)], Classifier s t)
classifyAndTrain c p = (bmu, diffs, c')
  where (bmu, _, diffs, c') = S.trainAndClassify c p
