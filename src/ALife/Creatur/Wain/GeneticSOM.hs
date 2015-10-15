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
    _patternMap,
    _exponentialParams,
    _tweaker,
    patternMap,
    exponentialParams,
    tweaker,
    Label,
    Difference,
    Tweaker(..),
    r0Range,
    dRange,
    buildGeneticSOM,
    numModels,
    modelMap,
    hasModel,
    counterMap,
    classify,
    train,
    RandomExponentialParams(..),
    randomExponentialParams,
    randomExponential,
    maxSize,
    currentLearningRate,
    schemaQuality,
    discrimination
  ) where

import ALife.Creatur.Wain.GeneticSOMInternal
