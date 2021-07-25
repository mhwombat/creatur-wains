------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.GeneticSOM
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
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
    -- * Construction
    GeneticSOM,
    buildGeneticSOM,
    -- * Deconstruction
    maxSize,
    numModels,
    modelMap,
    counterMap,
    hasLabel,
    modelAt,
    currentLearningRate,
    schemaQuality,
    discrimination,
    -- * Learning parameters
    LearningParams,
    mkLearningParams,
    LearningParamRanges(..),
    widestLearningParamRanges,
    randomLearningParams,
    r0Range,
    rfRange,
    tfRange,
    -- * Lenses
    patternMap,
    _patternMap,
    learningParams,
    _learningParams,
    tweaker,
    _tweaker,
    -- * Learning and classification
    -- classify,
    trainAndClassify,
    imprint,
    -- * Other
    Label,
    Difference,
    Tweaker(..),
    ClassificationDetail(..),
    prettyClassificationDetail,
    prettyClassificationMoreDetail,
    ImprintDetail(..),
    prettyImprintDetail,
    filterByPattern
  ) where

import           ALife.Creatur.Wain.GeneticSOMInternal
