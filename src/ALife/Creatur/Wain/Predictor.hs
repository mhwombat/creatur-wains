------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Predictor
-- Copyright   :  (c) Amy de Buitléir 2013-2016
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
    buildPredictor,
    evaluateResponse,
    imprintOrReinforce,
    decisionReport
  ) where

import ALife.Creatur.Wain.PredictorInternal
import ALife.Creatur.Wain.GeneticSOM (Label)
