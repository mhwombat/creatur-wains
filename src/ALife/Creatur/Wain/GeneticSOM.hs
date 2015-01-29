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
    GeneticSOM,
    Label,
    r0Range,
    dRange,
    patternMap,
    counterMap,
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
    learningFunction,
    currentLearningRate,
    schemaQuality
  ) where

import ALife.Creatur.Wain.GeneticSOMInternal
