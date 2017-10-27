------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.GeneticSOM
-- Copyright   :  (c) Amy de Buitléir 2013-2017
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
    LearningParams,
    mkLearningParams,
    GeneticSOM,
    _patternMap,
    _learningParams,
    _tweaker,
    patternMap,
    learningParams,
    tweaker,
    Label,
    Difference,
    Tweaker(..),
    r0Range,
    rfRange,
    tfRange,
    buildGeneticSOM,
    numModels,
    modelMap,
    hasLabel,
    counterMap,
    classify,
    trainAndClassify,
    train,
    RandomLearningParams(..),
    randomLearningParams,
    randomLearningFunction,
    maxSize,
    currentLearningRate,
    schemaQuality,
    discrimination
  ) where

import ALife.Creatur.Wain.GeneticSOMInternal
