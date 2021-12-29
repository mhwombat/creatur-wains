------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PredictorQC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
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
    arbTestPredictor,
    arbEmptyTestPredictor,
    TrainingTestData(..),
    ImprintTestData(..)
  ) where

import qualified ALife.Creatur.Gene.Numeric.PlusMinusOne as PM1
import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import qualified ALife.Creatur.Gene.Test                 as GT
import           ALife.Creatur.Wain.GeneticSOM           (Label,
                                                          trainAndClassify)
import           ALife.Creatur.Wain.GeneticSOMQC         (sizedArbEmptyGeneticSOM,
                                                          sizedArbGeneticSOM)
import           ALife.Creatur.Wain.PredictorInternal
-- import ALife.Creatur.Wain.Pretty (Pretty(pretty))
import           ALife.Creatur.Wain.Response             (Response (..))
import           ALife.Creatur.Wain.ResponseQC           (TestAction,
                                                          TestResponse,
                                                          TestResponseAdjuster,
                                                          arbTestResponse)
import           Control.DeepSeq                         (deepseq)
import qualified Data.Datamining.Clustering.SGM4Internal as SOM
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)
import           Test.QuickCheck.Counterexamples         hiding (labels)

type TestPredictor = Predictor TestResponseAdjuster TestAction

sizedArbTestPredictor :: Int -> Gen TestPredictor
sizedArbTestPredictor n = do
  ~[nObjects, nConditions, capacity] <- GT.divvy n 3
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

data TrainingTestData
  = TrainingTestData
      {
        xPredictor :: TestPredictor,
        xResponse  :: TestResponse,
        xOutcomes  :: [PM1.PM1Double]
      } deriving (Eq, Show)

sizedArbTrainingTestData :: Int -> Gen TrainingTestData
sizedArbTrainingTestData n = do
  ~[nO, nConditions, capacity] <- GT.divvy n 3
  let nObjects = min 3 nO
  p <- arbTestPredictor nObjects nConditions capacity
  r <- arbTestResponse nObjects nConditions
  os <- vectorOf nConditions arbitrary
  return $ TrainingTestData p r os

instance Arbitrary TrainingTestData where
  arbitrary = sized sizedArbTrainingTestData

prop_predict_never_causes_error
  :: TrainingTestData -> UI.UIDouble -> Bool
prop_predict_never_causes_error (TrainingTestData p r _) prob
  = deepseq x True
  where x = predict p r prob

-- prop_prettyPredictionDetail_never_causes_error
--   :: TrainingTestData -> UI.UIDouble -> Bool
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

rawDiff :: [PM1.PM1Double] ->  [PM1.PM1Double] -> Double
rawDiff xs ys =
  sum $ zipWith (\x y -> abs $ PM1.wide x - PM1.wide y) xs ys

data ImprintTestData
  = ImprintTestData
      {
        iPredictor :: TestPredictor,
        iLabels    :: [Label],
        iAction    :: TestAction,
        iOutcomes  :: [PM1.PM1Double],
        iDeltas    :: [PM1.PM1Double]
      } deriving (Eq, Show)

sizedArbImprintTestData :: Int -> Gen ImprintTestData
sizedArbImprintTestData n = do
  ~[nO, nConditions, capacity] <- GT.divvy n 3
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
  SOM.numModels p < SOM.maxSize p ==> diffAfter <= diffBefore
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
