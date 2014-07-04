------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.GeneticSOM
-- Copyright   :  (c) Amy de Buitléir 2013-2014
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
    GeneticSOM(..),
    Label,
    buildGeneticSOM,
    numModels,
    models,
    toList,
    justClassify,
    reportAndTrain,
    learn,
    RandomDecayingGaussianParams(..),
    randomDecayingGaussianParams,
    randomDecayingGaussian,
    somOK,
    learningFunction,
    size
  ) where

import ALife.Creatur.Wain.GeneticSOMInternal
