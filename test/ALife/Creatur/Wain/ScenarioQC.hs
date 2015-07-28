------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ScenarioQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
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
module ALife.Creatur.Wain.ScenarioQC
  (
    test,
    arbScenario,
    equivScenario
  ) where

import ALife.Creatur.Wain.Scenario
import ALife.Creatur.Wain.UnitIntervalQC (equivUIDoubleVector)
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.Weights (Weights)
import ALife.Creatur.Wain.WeightsQC (sizedArbWeights)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

sizedArbScenario :: Int -> Gen Scenario
sizedArbScenario n = do
  nObjects <- choose (0, n)
  let nConditions = n - nObjects
  arbScenario nObjects nConditions

-- This method is used by other test classes to ensure that all of the
-- scenarios have the same number of objects and condition parameters.
arbScenario :: Int -> Int -> Gen Scenario
arbScenario nObjects nConditions = do
  xs <- vectorOf nObjects arbitrary
  c <- vectorOf nConditions arbitrary
  return $ Scenario xs c

instance Arbitrary Scenario where
  arbitrary = sized sizedArbScenario

data TestData1 = TestData1 Weights Scenario Scenario
  deriving (Eq, Show)

sizedArbTestData1 :: Int -> Gen TestData1
sizedArbTestData1 n = do
  j <- choose(1,n+1)
  let k = n + 2 - j + 1
  cw <- sizedArbWeights k
  let x = Scenario (replicate j 0) (replicate k 0)
  let y = Scenario (replicate j 1) (replicate k 1)
  return $ TestData1 cw x y

instance Arbitrary TestData1 where
  arbitrary = sized sizedArbTestData1

prop_scenarioDiff_can_be_1 :: TestData1 -> Property
prop_scenarioDiff_can_be_1 (TestData1 cw x y) = property $ 1 - d < 1e-8
  where d = scenarioDiff cw x y

prop_scenarioDiff_can_be_0 :: Weights -> Scenario -> Property
prop_scenarioDiff_can_be_0 cw s = property $ abs (x - 0) < 1e-8
  where x = scenarioDiff cw s s

prop_scenarioDiff_in_range
  :: Weights -> Scenario -> Scenario -> Property
prop_scenarioDiff_in_range cw a b = property $ 0 <= x && x <= 1
  where x = scenarioDiff cw a b

-- prop_makeScenarioSimilar_works
--   :: Weights -> Scenario -> UIDouble -> Scenario -> Property
-- prop_makeScenarioSimilar_works cw
--   = prop_makeSimilar_works (scenarioDiff cw) makeScenarioSimilar

equivScenario :: Scenario -> Scenario -> Bool
equivScenario x y
  = _labels x == _labels y
      && equivUIDoubleVector (_condition x) (_condition y)

test :: Test
test = testGroup "ALife.Creatur.Wain.ScenarioQC"
  [
    testProperty "prop_serialize_round_trippable - Scenario"
      (prop_serialize_round_trippable :: Scenario -> Property),
    testProperty "prop_genetic_round_trippable - Scenario"
      (prop_genetic_round_trippable equivScenario
         :: Scenario -> Property),
    testProperty "prop_diploid_identity - Scenario"
      (prop_diploid_identity (==) :: Scenario -> Property),
    testProperty "prop_diploid_expressable - Scenario"
      (prop_diploid_expressable :: Scenario -> Scenario -> Property),
    testProperty "prop_diploid_readable - Scenario"
      (prop_diploid_readable :: Scenario -> Scenario -> Property),
    testProperty "prop_scenarioDiff_can_be_1"
      prop_scenarioDiff_can_be_1,
    testProperty "prop_scenarioDiff_can_be_0"
      prop_scenarioDiff_can_be_0,
    testProperty "prop_scenarioDiff_in_range"
      prop_scenarioDiff_in_range
    -- testProperty "prop_makeScenarioSimilar_works"
    --   prop_makeScenarioSimilar_works
  ]
