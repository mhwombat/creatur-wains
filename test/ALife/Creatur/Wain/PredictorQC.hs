------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PredictorQC
-- Copyright   :  (c) Amy de Buitléir 2013-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.PredictorQC
  (
    test,
    equivTweaker,
    equivPredictor,
    arbTestPredictor,
    arbEmptyTestPredictor,
    TrainingTestData(..),
    ImprintTestData(..)
  ) where

import           ALife.Creatur.Wain.GeneticSOMInternal
    (Label, maxSize, numModels, trainAndClassify)
import           ALife.Creatur.Wain.GeneticSOMQC
    (equivGSOM, sizedArbEmptyGeneticSOM, sizedArbGeneticSOM)
import           ALife.Creatur.Wain.PlusMinusOne
    (PM1Double, pm1ToDouble)
import           ALife.Creatur.Wain.PredictorInternal
-- import ALife.Creatur.Wain.Pretty (Pretty(pretty))
import           ALife.Creatur.Wain.Probability
    (Probability)
import           ALife.Creatur.Wain.Response
    (Response (..))
import           ALife.Creatur.Wain.ResponseQC
    (TestAction, TestResponse, arbTestResponse)
import           ALife.Creatur.Wain.SimpleResponseTweaker
    (ResponseTweaker (..), responseDiff)
import           ALife.Creatur.Wain.TestUtils
import           Control.DeepSeq
    (deepseq)
import           Test.Framework
    (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2
    (testProperty)
import           Test.QuickCheck                          hiding
    (labels, maxSize)

type TestTweaker = ResponseTweaker TestAction

instance Arbitrary TestTweaker where
  arbitrary = return ResponseTweaker

equivTweaker :: TestTweaker -> TestTweaker -> Bool
equivTweaker _ _ = True

type TestPredictor = Predictor TestAction (ResponseTweaker TestAction)

sizedArbTestPredictor :: Int -> Gen TestPredictor
sizedArbTestPredictor n = do
  nObjects:nConditions:capacity:[] <- divvy n 3
  arbTestPredictor nObjects nConditions capacity

arbEmptyTestPredictor :: Int -> Gen TestPredictor
arbEmptyTestPredictor capacity = do
  sizedArbEmptyGeneticSOM capacity

arbTestPredictor :: Int -> Int -> Int -> Gen TestPredictor
arbTestPredictor nObjects nConditions capacity = do
  let genResponse = arbTestResponse nObjects nConditions
  sizedArbGeneticSOM genResponse capacity

instance Arbitrary TestPredictor where
  arbitrary = sized sizedArbTestPredictor

equivPredictor :: TestPredictor -> TestPredictor -> Bool
equivPredictor = equivGSOM equivTweaker

data TrainingTestData
  = TrainingTestData
      {
        xPredictor :: TestPredictor,
        xResponse  :: TestResponse,
        xOutcomes  :: [PM1Double]
      } deriving (Eq, Show)

sizedArbTrainingTestData :: Int -> Gen TrainingTestData
sizedArbTrainingTestData n = do
  nO:nConditions:capacity:[] <- divvy n 3
  let nObjects = min 3 nO
  p <- arbTestPredictor nObjects nConditions capacity
  r <- arbTestResponse nObjects nConditions
  os <- vectorOf nConditions arbitrary
  return $ TrainingTestData p r os

instance Arbitrary TrainingTestData where
  arbitrary = sized sizedArbTrainingTestData

prop_predict_never_causes_error
  :: TrainingTestData -> Probability -> Property
prop_predict_never_causes_error (TrainingTestData p r _) prob
  = property $ deepseq x True
  where x = predict p r prob

-- prop_prettyPredictionDetail_never_causes_error
--   :: TrainingTestData -> Probability -> Property
-- prop_prettyPredictionDetail_never_causes_error
--   (TrainingTestData p r _) prob
--   = property $ deepseq x' True
--   where x = predict p r prob
--         x' = pretty x

-- NOTE: I've tested this with maxSuccess=10000!
prop_training_makes_predictions_more_accurate
  :: TrainingTestData -> Property
prop_training_makes_predictions_more_accurate (TrainingTestData d r os)
  = property $ errAfter < 0.1 || errAfter <= errBefore
  where r2 = pResponse $ predict d r 1
        errBefore = rawDiff os (_outcomes r2)
        rActual = r { _outcomes = os }
        d2 = snd $ trainAndClassify d rActual
        r3 = pResponse $ predict d2 r 1
        errAfter = rawDiff os (_outcomes r3)

rawDiff :: [PM1Double] ->  [PM1Double] -> Double
rawDiff xs ys =
  sum $ zipWith (\x y -> abs $ pm1ToDouble x - pm1ToDouble y) xs ys

data ImprintTestData
  = ImprintTestData
      {
        iPredictor :: TestPredictor,
        iLabels    :: [Label],
        iAction    :: TestAction,
        iOutcomes  :: [PM1Double],
        iDeltas    :: [PM1Double]
      } deriving (Eq, Show)

sizedArbImprintTestData :: Int -> Gen ImprintTestData
sizedArbImprintTestData n = do
  nO:nConditions:capacity:[] <- divvy n 3
  let nObjects = min 3 nO
  p <- arbTestPredictor nObjects nConditions capacity
  ls <- vectorOf nObjects arbitrary
  a <- arbitrary
  os <- vectorOf nConditions $ choose (0.00001, 1)
  ds <- vectorOf nConditions $ choose (0.00001, 1)
  return $ ImprintTestData p ls a os ds

instance Arbitrary ImprintTestData where
  arbitrary = sized sizedArbImprintTestData

prop_imprintOrReinforce_never_causes_error
  :: ImprintTestData -> Property
prop_imprintOrReinforce_never_causes_error (ImprintTestData p ls a os ds)
  = property $ deepseq x True
  where x = imprintOrReinforce p ls a os ds

-- prop_prettyImprintReport_never_causes_error
--   :: ImprintTestData -> Property
-- prop_prettyImprintReport_never_causes_error
--   (ImprintTestData d r os ds _)
--   = property $ deepseq x' True
--   where (x, _)
--           = imprintOrReinforce d (view labels r) (view action r) os ds
--         x' = prettyLearningReport x

prop_imprintOrReinforce_works :: ImprintTestData -> Property
prop_imprintOrReinforce_works (ImprintTestData p ls a os ds) =
  numModels p < maxSize p ==> diffAfter <= diffBefore
  where r0 = Response ls a os
        rBefore = pResponse $ predict p r0 1
        (_, p2) = imprintOrReinforce p ls a os ds
        rAfter = pResponse $ predict p2 r0 1
        diffBefore = responseDiff rBefore r0
        diffAfter = responseDiff rAfter r0

prop_learn_never_causes_error
  :: ImprintTestData -> Property
prop_learn_never_causes_error (ImprintTestData p ls a os _)
  = property $ deepseq x True
  where x = learn p r
        r = Response ls a os

-- prop_prettyLearningReport_never_causes_error
--   :: ImprintTestData -> Property
-- prop_prettyLearningReport_never_causes_error
--   (ImprintTestData d r _ _ _)
--   = property $ deepseq x' True
--   where (x, _) = learn d r
--         x' = prettyLearningReport x

test :: Test
test = testGroup "ALife.Creatur.Wain.PredictorQC"
  [
    testProperty "prop_serialize_round_trippable - Tweaker"
      (prop_serialize_round_trippable :: TestTweaker -> Property),
    testProperty "prop_genetic_round_trippable - Tweaker"
      (prop_genetic_round_trippable equivTweaker
        :: TestTweaker -> Property),
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
    testProperty "prop_predict_never_causes_error"
      prop_predict_never_causes_error,
    -- testProperty "prop_prettyPredictionDetail_never_causes_error"
    --   prop_prettyPredictionDetail_never_causes_error,
    testProperty "prop_training_makes_predictions_more_accurate"
      prop_training_makes_predictions_more_accurate,
    testProperty "prop_imprintOrReinforce_never_causes_error"
      prop_imprintOrReinforce_never_causes_error,
    -- testProperty "prop_prettyImprintReport_never_causes_error"
    --   prop_prettyImprintReport_never_causes_error,
    testProperty "prop_imprintOrReinforce_works"
      prop_imprintOrReinforce_works,
    testProperty "prop_learn_never_causes_error"
      prop_learn_never_causes_error
    -- testProperty "prop_prettyLearningReport_never_causes_error"
    --   prop_prettyLearningReport_never_causes_error
  ]
