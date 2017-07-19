------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Runs the QuickCheck tests.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur.WainQC (test)
import ALife.Creatur.Wain.BrainQC (test)
import ALife.Creatur.Wain.ClassifierQC (test)
import ALife.Creatur.Wain.PredictorQC (test)
import ALife.Creatur.Wain.GeneticSOMQC (test)
import ALife.Creatur.Wain.SimpleMuserQC (test)
import ALife.Creatur.Wain.PlusMinusOneQC (test)
import ALife.Creatur.Wain.ProbabilityQC (test)
import ALife.Creatur.Wain.ResponseQC (test)
import ALife.Creatur.Wain.SimpleResponseTweakerQC (test)
import ALife.Creatur.Wain.UnitIntervalQC (test)
import ALife.Creatur.Wain.UtilQC (test)
import ALife.Creatur.Wain.WeightsQC (test)

import Test.Framework as TF (defaultMain, Test)

tests :: [TF.Test]
tests =
  [
    -- In increasing order of complexity
    ALife.Creatur.Wain.UtilQC.test,
    ALife.Creatur.Wain.UnitIntervalQC.test,
    ALife.Creatur.Wain.PlusMinusOneQC.test,
    ALife.Creatur.Wain.WeightsQC.test,
    ALife.Creatur.Wain.ProbabilityQC.test,
    ALife.Creatur.Wain.ResponseQC.test,
    ALife.Creatur.Wain.SimpleResponseTweakerQC.test,
    ALife.Creatur.Wain.SimpleMuserQC.test,
    ALife.Creatur.Wain.GeneticSOMQC.test,
    ALife.Creatur.Wain.ClassifierQC.test,
    ALife.Creatur.Wain.PredictorQC.test,
    ALife.Creatur.Wain.BrainQC.test,
    ALife.Creatur.WainQC.test
  ]

main :: IO ()
main = defaultMain tests
