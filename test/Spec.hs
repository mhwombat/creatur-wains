------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Runs the QuickCheck tests.
--
------------------------------------------------------------------------
import ALife.Creatur.Wain.BrainQC          (test)
import ALife.Creatur.Wain.ClassifierQC     (test)
import ALife.Creatur.Wain.GeneticSOMQC     (test)
import ALife.Creatur.Wain.LearningParamsQC (test)
import ALife.Creatur.Wain.PatternQC        (test)
import ALife.Creatur.Wain.PredictorQC      (test)
import ALife.Creatur.Wain.ProbabilityQC    (test)
import ALife.Creatur.Wain.ResponseQC       (test)
import ALife.Creatur.Wain.SimpleMuserQC    (test)
import ALife.Creatur.WainQC                (test)

import Test.Framework                      qualified as TF

tests :: [TF.Test]
tests =
  [
    -- In increasing order of complexity
    ALife.Creatur.Wain.PatternQC.test,
    ALife.Creatur.Wain.ProbabilityQC.test,
    ALife.Creatur.Wain.ResponseQC.test,
    ALife.Creatur.Wain.SimpleMuserQC.test,
    ALife.Creatur.Wain.LearningParamsQC.test,
    ALife.Creatur.Wain.GeneticSOMQC.test,
    ALife.Creatur.Wain.ClassifierQC.test,
    ALife.Creatur.Wain.PredictorQC.test,
    ALife.Creatur.Wain.BrainQC.test,
    ALife.Creatur.WainQC.test
  ]

main :: IO ()
main = TF.defaultMain tests
