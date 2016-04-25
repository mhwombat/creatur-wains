------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Predictor
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A prediction model based on a Kohonen Self-organising Map (SOM).
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Predictor
  (
    Label,
    Predictor,
    PredictorTweaker(..),
    buildPredictor,
    predict,
    scenarios,
    hasScenario,
    imprintOrReinforce
  ) where

import ALife.Creatur.Wain.PredictorInternal
import ALife.Creatur.Wain.GeneticSOM (Label)
