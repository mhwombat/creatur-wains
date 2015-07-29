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
    Signature,
    Tweaker(..),
    exponentialParams,
    tweaker,
    r0Range,
    dRange,
    buildGeneticSOM,
    numModels,
    modelMap,
    counterMap,
    classify,
    train,
    RandomExponentialParams(..),
    randomExponentialParams,
    randomExponential,
    somOK,
    currentLearningRate,
    schemaQuality,
    discrimination
  ) where

import ALife.Creatur.Wain.GeneticSOMInternal
