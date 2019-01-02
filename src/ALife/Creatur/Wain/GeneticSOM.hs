------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.GeneticSOM
-- Copyright   :  (c) Amy de Buitléir 2013-2019
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
    Label,
    Difference,
    LearningParams,
    mkLearningParams,
    LearningParamRanges(..),
    widestLearningParamRanges,
    randomLearningParams,
    r0Range,
    rfRange,
    tfRange,
    Tweaker(..),
    GeneticSOM,
    patternMap,
    _patternMap,
    learningParams,
    _learningParams,
    tweaker,
    _tweaker,
    buildGeneticSOM,
    maxSize,
    numModels,
    modelMap,
    counterMap,
    hasLabel,
    modelAt,
    currentLearningRate,
    schemaQuality,
    discrimination,
    diffThreshold,
    ClassificationDetail(..),
    prettyClassificationDetail,
    prettyClassificationMoreDetail,
    -- classify,
    trainAndClassify
  ) where

import ALife.Creatur.Wain.GeneticSOMInternal
