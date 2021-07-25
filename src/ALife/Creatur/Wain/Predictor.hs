------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Predictor
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
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
    ClassificationDetail(..),
    PredictionDetail(..),
    LearningReport(..),
    buildPredictor,
    predict,
    learn,
    actions,
    scenarios,
    hasScenario,
    imprintOrReinforce,
    prettyLearningReport,
    filterLabels
  ) where

import           ALife.Creatur.Wain.GeneticSOM
    (ClassificationDetail (..), Label)
import           ALife.Creatur.Wain.PredictorInternal
