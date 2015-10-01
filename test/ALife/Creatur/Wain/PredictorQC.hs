------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PredictorQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.PredictorQC
  (
    test,
    equivTweaker,
    equivPredictor,
    arbTestPredictor,
    --
    TrainingTestData(..)
  ) where

import ALife.Creatur.Wain.Predictor
import ALife.Creatur.Wain.GeneticSOM (train)
import ALife.Creatur.Wain.GeneticSOMQC (equivGSOM)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, pm1ToDouble)
import ALife.Creatur.Wain.Response (Response(..))
import ALife.Creatur.Wain.ResponseQC (TestAction, TestResponse,
  arbTestResponse)
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.GeneticSOMInternal (patternMap)
import ALife.Creatur.Wain.GeneticSOMQC (sizedArbGeneticSOM)
import Control.Lens
import Data.Datamining.Clustering.SOSInternal (diffThreshold)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type TestTweaker = PredictorTweaker TestAction

instance Arbitrary TestTweaker where
  arbitrary = return PredictorTweaker

equivTweaker :: TestTweaker -> TestTweaker -> Bool
equivTweaker _ _ = True

type TestPredictor = Predictor TestAction

sizedArbTestPredictor :: Int -> Gen TestPredictor
sizedArbTestPredictor n = do
  nObjects <- choose (0, n)
  nConditions <- choose (0, n - nObjects)
  let nSize = n - nObjects - nConditions
  arbTestPredictor nObjects nConditions nSize

arbTestPredictor :: Int -> Int -> Int -> Gen TestPredictor
arbTestPredictor nObjects nConditions nSize = do
  let genResponse = arbTestResponse nObjects nConditions
  sizedArbGeneticSOM genResponse nSize

instance Arbitrary TestPredictor where
  arbitrary = sized sizedArbTestPredictor

equivPredictor :: TestPredictor -> TestPredictor -> Bool
equivPredictor = equivGSOM equivTweaker

data TrainingTestData
  = TrainingTestData
      {
        xPredictor :: TestPredictor,
        xResponse :: TestResponse,
        xOutcomes :: [PM1Double]
      } deriving (Eq, Show)

sizedArbTrainingTestData :: Int -> Gen TrainingTestData
sizedArbTrainingTestData n = do
  nSize <- choose (1, max 1 n)
  nObjects <- choose (1, min 3 (max 1 (n - nSize)))
  let nConditions = max 1 (n - nSize - nObjects)
  p <- arbTestPredictor nObjects nConditions nSize
  let pm = view patternMap p
  let pm' = pm { diffThreshold=0.1 }
  let p' = set patternMap pm' p
  r <- arbTestResponse nObjects nConditions
  os <- vectorOf nConditions arbitrary
  return $ TrainingTestData p' r os

instance Arbitrary TrainingTestData where
  arbitrary = sized sizedArbTrainingTestData

-- NOTE: I've tested this with maxSuccess=10000!
prop_training_makes_predictions_more_accurate
  :: TrainingTestData -> Property
prop_training_makes_predictions_more_accurate (TrainingTestData d r os)
  = property $ errAfter < 0.1 || errAfter <= errBefore
  where (r2, _, _, d2) = predict d r 1
        errBefore = rawDiff os (_outcomes r2)
        rActual = r { _outcomes = os }
        d3 = train d2 rActual
        (r4, _, _, _) = predict d3 r 1
        errAfter = rawDiff os (_outcomes r4)

rawDiff :: [PM1Double] ->  [PM1Double] -> Double
rawDiff xs ys =
  sum $ zipWith (\x y -> abs $ pm1ToDouble x - pm1ToDouble y) xs ys

test :: Test
test = testGroup "ALife.Creatur.Wain.PredictorQC"
  [
    testProperty "prop_serialize_round_trippable - Tweaker"
      (prop_serialize_round_trippable :: TestTweaker -> Property),
    testProperty "prop_genetic_round_trippable - Tweaker"
      (prop_genetic_round_trippable equivTweaker :: TestTweaker -> Property),
    testProperty "prop_diploid_identity - Tweaker"
      (prop_diploid_identity (==) :: TestTweaker -> Property),
    testProperty "prop_diploid_expressable - Tweaker"
      (prop_diploid_expressable :: TestTweaker -> TestTweaker -> Property),
    testProperty "prop_diploid_readable - Tweaker"
      (prop_diploid_readable :: TestTweaker -> TestTweaker -> Property),
    testProperty "prop_serialize_round_trippable - Predictor"
      (prop_serialize_round_trippable :: TestPredictor -> Property),
    testProperty "prop_genetic_round_trippable - Predictor"
      (prop_genetic_round_trippable equivPredictor :: TestPredictor -> Property),
    testProperty "prop_diploid_identity - Predictor"
      (prop_diploid_identity (==) :: TestPredictor -> Property),
    testProperty "prop_diploid_expressable - Predictor"
      (prop_diploid_expressable :: TestPredictor -> TestPredictor -> Property),
    testProperty "prop_diploid_readable - Predictor"
      (prop_diploid_readable :: TestPredictor -> TestPredictor -> Property),
    testProperty "prop_training_makes_predictions_more_accurate"
      prop_training_makes_predictions_more_accurate
  ]
