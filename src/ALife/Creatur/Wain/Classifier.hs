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
--   Returns the "signature" (differences between the input pattern
--   and each model in the classifier) of each input pattern,
--   and the updated classifier.
classifySetAndTrain
  :: Classifier s t -> [s]
    -> ([S.Label], [[(S.Label, S.Difference)]], Classifier s t)
classifySetAndTrain c ps = (reverse bmus, reverse diffs, c')
  where (bmus, diffs, c') = foldl' classifyNextAndTrain ([], [], c) ps

classifyNextAndTrain
  :: ([S.Label], [[(S.Label, S.Difference)]], Classifier s t)
    -> s -> ([S.Label], [[(S.Label, S.Difference)]], Classifier s t)
classifyNextAndTrain (bmus, diffs, c) p = (bmu:bmus, d:diffs, c')
  where (bmu, d, c') = classifyAndTrain c p

-- | Updates the classifier models based on the input pattern.
--   Returns the "signature" (differences between the input pattern
--   and each model in the classifier) and the updated classifier.
classifyAndTrain
  :: Classifier s t -> s
    -> (S.Label, [(S.Label, S.Difference)], Classifier s t)
classifyAndTrain c p = (bmu, diffs, c')
  where (bmu, _, diffs, c') = S.trainAndClassify c p
