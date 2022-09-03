------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PredictorQC
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.PredictorQC
  (
    test,
    sizedArbTestPredictor,
    TrainingTestData(..),
    ImprintTestData(..)
  ) where

import ALife.Creatur.Gene.Numeric.PlusMinusOne qualified as PM1
import ALife.Creatur.Gene.Numeric.UnitInterval qualified as UI
import ALife.Creatur.Gene.Test                 qualified as GT
import ALife.Creatur.Wain.GeneticSOM           (Label, trainAndClassify)
import ALife.Creatur.Wain.GeneticSOMQC         (arbGeneticSOM)
import ALife.Creatur.Wain.PredictorInternal
-- import ALife.Creatur.Wain.Pretty (Pretty(pretty))
import ALife.Creatur.Wain.ResponseInternal     (Response (..), sizedArbResponse)
import ALife.Creatur.Wain.ResponseQC           (TestAction, TestResponse,
                                                TestResponseAdjuster)
import Control.DeepSeq                         (deepseq)
import Data.Datamining.Clustering.SGM4         qualified as SOM
import Test.Framework                          (Test, testGroup)
import Test.Framework.Providers.QuickCheck2    (testProperty)
import Test.QuickCheck.Counterexamples         hiding (labels)

type TestPredictor = Predictor TestResponseAdjuster TestAction

sizedArbTestPredictor :: Int -> Int -> Gen TestPredictor
sizedArbTestPredictor nObjects nConditions = do
  let genResponse = sizedArbResponse nObjects nConditions arbitrary
  arbGeneticSOM genResponse

instance Arbitrary TestPredictor where
  arbitrary = do
    nObjects <- fmap (min 3) getSize
    nConditions <- fmap (min 5) getSize
    sizedArbTestPredictor nObjects nConditions

data TrainingTestData
  = TrainingTestData
      {
        xPredictor :: TestPredictor,
        xResponse  :: TestResponse,
        xOutcomes  :: [PM1.Double]
      } deriving (Eq, Show)

instance Arbitrary TrainingTestData where
  arbitrary = do
    nObjects <- fmap (min 3) getSize
    nConditions <- fmap (min 5) getSize
    p <- sizedArbTestPredictor nObjects nConditions
    r <- sizedArbResponse nObjects nConditions arbitrary
    os <- vectorOf nConditions arbitrary
    return $ TrainingTestData p r os

prop_predict_never_causes_error
  :: TrainingTestData -> UI.Double -> Bool
prop_predict_never_causes_error (TrainingTestData p r _) prob
  = deepseq x True
  where x = predict p r prob

-- prop_prettyPredictionDetail_never_causes_error
--   :: TrainingTestData -> UI.Double -> Bool
-- prop_prettyPredictionDetail_never_causes_error
--   (TrainingTestData p r _) prob
--   = deepseq x' True
--   where x = predict p r prob
--         x' = pretty x

-- NOTE: I've tested this with maxSuccess=10000!
prop_training_makes_predictions_more_accurate
  :: TrainingTestData -> Bool
prop_training_makes_predictions_more_accurate (TrainingTestData d r os)
  = errAfter < 0.1 || errAfter <= errBefore
  where r2 = pResponse $ predict d r 1
        errBefore = rawDiff os (outcomes r2)
        rActual = r { outcomes = os }
        d2 = snd $ trainAndClassify d rActual
        r3 = pResponse $ predict d2 r 1
        errAfter = rawDiff os (outcomes r3)

rawDiff :: [PM1.Double] ->  [PM1.Double] -> Double
rawDiff xs ys =
  sum $ zipWith (\x y -> abs $ PM1.wide x - PM1.wide y) xs ys

data ImprintTestData
  = ImprintTestData
      {
        iPredictor :: TestPredictor,
        iLabels    :: [Label],
        iAction    :: TestAction,
        iOutcomes  :: [PM1.Double],
        iDeltas    :: [PM1.Double]
      } deriving (Eq, Show)

sizedArbImprintTestData :: Int -> Int -> Gen ImprintTestData
sizedArbImprintTestData nObjects nConditions = do
  p <- sizedArbTestPredictor nObjects nConditions
  ls <- vectorOf nObjects arbitrary
  a <- arbitrary
  os <- vectorOf nConditions $ choose (0.00001, 1)
  ds <- vectorOf nConditions $ choose (0.00001, 1)
  return $ ImprintTestData p ls a os ds

instance Arbitrary ImprintTestData where
  arbitrary = do
    nObjects <- fmap (min 3) getSize
    nConditions <- fmap (min 5) getSize
    sizedArbImprintTestData nObjects nConditions

prop_imprintOrReinforce_never_causes_error
  :: ImprintTestData -> Bool
prop_imprintOrReinforce_never_causes_error (ImprintTestData p ls a os ds)
  = deepseq x True
  where x = imprintOrReinforce p ls a os ds

-- prop_prettyImprintReport_never_causes_error
--   :: ImprintTestData -> Bool
-- prop_prettyImprintReport_never_causes_error
--   (ImprintTestData d r os ds _)
--   = deepseq x' True
--   where (x, _)
--           = imprintOrReinforce d (view labels r) (view action r) os ds
--         x' = prettyLearningReport x

prop_imprintOrReinforce_works :: ImprintTestData -> Property
prop_imprintOrReinforce_works (ImprintTestData p ls a os ds) =
  not (SOM.atCapacity p) ==> diffAfter <= diffBefore
  where r0 = Response ls a os
        rBefore = pResponse $ predict p r0 1
        (_, p2) = imprintOrReinforce p ls a os ds
        rAfter = pResponse $ predict p2 r0 1
        diffBefore = SOM.difference (SOM.adjuster p) rBefore r0
        diffAfter = SOM.difference (SOM.adjuster p) rAfter r0

prop_learn_never_causes_error
  :: ImprintTestData -> Bool
prop_learn_never_causes_error (ImprintTestData p ls a os _)
  = deepseq x True
  where x = learn p r
        r = Response ls a os

-- prop_prettyLearningReport_never_causes_error
--   :: ImprintTestData -> Bool
-- prop_prettyLearningReport_never_causes_error
--   (ImprintTestData d r _ _ _)
--   = deepseq x' True
--   where (x, _) = learn d r
--         x' = prettyLearningReport x

test :: Test
test = testGroup "ALife.Creatur.Wain.PredictorQC"
  [
    testProperty "prop_serialize_round_trippable - Predictor"
      (GT.prop_serialize_round_trippable :: TestPredictor -> Bool),
    testProperty "prop_genetic_round_trippable - Predictor"
      (GT.prop_genetic_round_trippable (==) :: TestPredictor -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - Predictor"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestPredictor -> Bool),
    testProperty "prop_diploid_identity - Predictor"
      (GT.prop_diploid_identity (==) :: TestPredictor -> Bool),
    -- testProperty "prop_show_read_round_trippable - Predictor"
    --   (prop_show_read_round_trippable (==) :: TestPredictor -> Bool),
    testProperty "prop_diploid_expressable - Predictor"
      (GT.prop_diploid_expressable :: TestPredictor -> TestPredictor -> Bool),
    testProperty "prop_diploid_readable - Predictor"
      (GT.prop_diploid_readable :: TestPredictor -> TestPredictor -> Bool),

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
