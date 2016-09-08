------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PredictorQC
-- Copyright   :  (c) Amy de Buitléir 2013-2016
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
    arbEmptyTestPredictor,
    -- TrainingTestData(..),
    ImprintTestData(..)
  ) where

import ALife.Creatur.Wain.PredictorInternal
import ALife.Creatur.Wain.GeneticSOMQC (equivGSOM)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double)
import ALife.Creatur.Wain.Response (Response(..), labels, action,
  outcomes)
import ALife.Creatur.Wain.ResponseQC (TestAction, TestResponse,
  arbTestResponse)
import ALife.Creatur.Wain.SimpleResponseTweaker (ResponseTweaker(..))
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.GeneticSOMInternal (patternMap, numModels,
  maxSize, modelMap)
import ALife.Creatur.Wain.GeneticSOMQC (sizedArbGeneticSOM,
  sizedArbEmptyGeneticSOM)
import Control.Lens
import Data.Datamining.Clustering.SGMInternal (diffThreshold)
import Data.Map.Strict ((!))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding (labels, maxSize)

type TestTweaker = ResponseTweaker TestAction

instance Arbitrary TestTweaker where
  arbitrary = return ResponseTweaker

equivTweaker :: TestTweaker -> TestTweaker -> Bool
equivTweaker _ _ = True

type TestPredictor = Predictor TestAction (ResponseTweaker TestAction)

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

arbEmptyTestPredictor :: Int -> Gen TestPredictor
arbEmptyTestPredictor nSize = sizedArbEmptyGeneticSOM nSize

-- data TrainingTestData
--   = TrainingTestData
--       {
--         xPredictor :: TestPredictor,
--         xWeights :: Weights,
--         xDefaultOutcomes :: [PM1Double],
--         xLdss :: [[(Cl.Label, Cl.Difference)]],
--         xAction :: TestAction
--       } deriving (Eq, Show)

-- sizedArbTrainingTestData :: Int -> Gen TrainingTestData
-- sizedArbTrainingTestData n = do
--   nSize <- choose (1, max 1 n)
--   nObjects <- choose (1, min 3 (max 1 (n - nSize)))
--   let nConditions = max 1 (n - nSize - nObjects)
--   p <- arbTestPredictor nObjects nConditions nSize
--   ws <- sizedArbWeights (nObjects + 1)
--   let pm = view patternMap p
--   let pm' = pm { diffThreshold=0.1 }
--   let p' = set patternMap pm' p
--   dos <- vectorOf nConditions arbitrary
--   ldss <- vectorOf nObjects arbitrary
--   a <- arbitrary
--   return $ TrainingTestData p' ws dos ldss a

-- instance Arbitrary TrainingTestData where
--   arbitrary = sized sizedArbTrainingTestData

-- prop_evaluateResponse_extrapolates_for_new_action
--   :: TrainingTestData -> TestAction -> Property
-- prop_evaluateResponse_extrapolates_for_new_action
--   (TrainingTestData p ws _ ldss a1) a2 =
--     a1 `elem` knownActions && not (a2 `elem` knownActions) ==>
--       and (zipWith (\a b -> abs a < abs b) os2 os1)
--   where knownActions = map _action . elems . modelMap $ p
--         (_, os1) = evaluateResponse ws testActionDiff2 undefined p ldss a1
--         (_, os2) = evaluateResponse ws testActionDiff2 undefined p ldss a2
--         -- have to use testActionDiff2!!!

data ImprintTestData
  = ImprintTestData
      {
        iPredictor :: TestPredictor,
        iResponse :: TestResponse,
        iOutcomes :: [PM1Double],
        iDeltas :: [PM1Double]
      } deriving (Eq, Show)

sizedArbImprintTestData :: Int -> Gen ImprintTestData
sizedArbImprintTestData n = do
  nSize <- choose (1, max 1 n)
  nObjects <- choose (1, min 3 (max 1 (n - nSize)))
  let nConditions = max 1 (n - nSize - nObjects)
  p <- arbTestPredictor nObjects nConditions nSize
  let pm = view patternMap p
  let pm' = pm { diffThreshold=0.1 }
  let p' = set patternMap pm' p
  r <- arbTestResponse nObjects nConditions
  os <- vectorOf nConditions $ choose (0.00001, 1)
  ds <- vectorOf nConditions $ choose (0.00001, 1)
  return $ ImprintTestData p' r os ds

instance Arbitrary ImprintTestData where
  arbitrary = sized sizedArbImprintTestData

prop_imprintOrReinforce_works :: ImprintTestData -> Property
prop_imprintOrReinforce_works (ImprintTestData p r os ds) =
  numModels p < maxSize p ==>
    and $ zipWith (>) (_outcomes r2) (_outcomes r1)
  where os0 = map (const (-1)) os
        r0 = set outcomes os0 r
        (bmu0, p1) = classifyAndMaybeCreateNewModel p r0
        r1 = modelMap p1 ! bmu0
        (_, _, p2) = imprintOrReinforce p (view labels r) (view action r) os ds
        (bmu2, p3) = classifyAndMaybeCreateNewModel p2 r0
        r2 = modelMap p3 ! bmu2

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
    testProperty "prop_imprintOrReinforce_works"
      prop_imprintOrReinforce_works
  ]
