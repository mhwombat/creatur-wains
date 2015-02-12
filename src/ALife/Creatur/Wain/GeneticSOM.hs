------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.GeneticSOM
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides a self-organising map that can be genetically configured.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.GeneticSOM
  (
    ExponentialParams(..),
    GeneticSOM,
    Label,
    Thinker(..),
    patternMap,
    counterMap,
    exponentialParams,
    teacher,
    r0Range,
    dRange,
    buildGeneticSOM,
    numModels,
    models,
    modelAt,
    toList,
    justClassify,
    reportAndTrain,
    learn,
    RandomExponentialParams(..),
    randomExponentialParams,
    randomExponential,
    somOK,
    currentLearningRate,
    schemaQuality
  ) where

import ALife.Creatur.Wain.GeneticSOMInternal
