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
    arbTestPredictor
  ) where

import ALife.Creatur.Wain.Predictor
import ALife.Creatur.Wain.GeneticSOM (train)
import ALife.Creatur.Wain.GeneticSOMQC (equivGSOM)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, pm1ToDouble)
import ALife.Creatur.Wain.Response (Response(..))
import ALife.Creatur.Wain.ResponseQC (TestAction, TestResponse,
  arbTestResponse)
import ALife.Creatur.Wain.Scenario (Scenario)
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.GeneticSOMInternal (patternMap)
import ALife.Creatur.Wain.GeneticSOMQC (sizedArbGeneticSOM)
import ALife.Creatur.Wain.WeightsQC (equivWeights)
import Control.Lens
import Data.Datamining.Clustering.SOSInternal (diffThreshold)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type TestTweaker = PredictorTweaker TestAction

instance Arbitrary TestTweaker where
  arbitrary
    = PredictorTweaker <$> arbitrary <*> arbitrary

equivTweaker :: TestTweaker -> TestTweaker -> Bool
equivTweaker (PredictorTweaker as bs) (PredictorTweaker xs ys) =
  equivWeights as xs && equivWeights bs ys

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
  = TrainingTestData TestPredictor TestResponse PM1Double
    deriving (Eq, Show)

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
  o <- arbitrary
  return $ TrainingTestData p' r o

instance Arbitrary TrainingTestData where
  arbitrary = sized sizedArbTrainingTestData

-- Note: I've tested this with maxSuccess=10000!
prop_training_makes_predictions_more_accurate
  :: TrainingTestData -> Property
prop_training_makes_predictions_more_accurate (TrainingTestData d r o) =
  property $ errAfter < 0.1 || errAfter <= errBefore
  where (r2, _, d2) = predict d (r, 1)
        predictionBefore = _outcome r2
        errBefore = abs (pm1ToDouble o - pm1ToDouble predictionBefore)
        rActual = r { _outcome = o }
        d3 = train d2 rActual
        (r4, _, _) = predict d3 (r, 1)
        predictionAfter = _outcome r4
        errAfter = abs (pm1ToDouble o - pm1ToDouble predictionAfter)

prop_imprint_works
  :: TestPredictor -> Scenario -> TestAction -> Property
prop_imprint_works d s a = property $ _outcome r' >= _outcome r
  where d' = imprint d s a
        r0 = Response s a 0
        (r, _, _) = predict d (r0, 1)
        (r', _, _) = predict d' (r0, 1)

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
      prop_training_makes_predictions_more_accurate,
    testProperty "prop_imprint_works"
      prop_imprint_works
  ]
