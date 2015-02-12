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
    test
  ) where

import ALife.Creatur.Wain.Scenario
import ALife.Creatur.Wain.ConditionQC ()
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.UnitIntervalQC ()
import ALife.Creatur.Wain.Util (unitInterval)
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.Weights (Weights)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

sizedArbScenario :: Int -> Gen Scenario
sizedArbScenario n = do
  is <- vectorOf n (arb8BitDouble unitInterval)
  os <- vectorOf n (arb8BitDouble unitInterval)
  c <- arbitrary
  return $ Scenario is os c
  
instance Arbitrary Scenario where
  arbitrary = sized sizedArbScenario

-- prop_scenarioDiff_can_be_1 :: Weights -> Property
-- prop_scenarioDiff_can_be_1 w = not (null ws) ==> abs (x - 1) < 1e-8
--   where x = scenarioDiff w (Scenario [0] [0] (Condition 0 0 0))
--               (Scenario [1] [1] (Condition 1 1 1))
--         ws = toDoubles w

prop_scenarioDiff_can_be_0 :: Weights -> Weights -> Scenario -> Property
prop_scenarioDiff_can_be_0 cw sw s = property $ abs (x - 0) < 1e-8
  where x = scenarioDiff cw sw s s

prop_scenarioDiff_in_range
  :: Weights -> Weights -> Scenario -> Scenario -> Property
prop_scenarioDiff_in_range cw sw a b = property $ 0 <= x && x <= 1
  where x = scenarioDiff cw sw a b

prop_makeScenarioSimilar_works
  :: Weights -> Weights -> Scenario -> UIDouble -> Scenario -> Property
prop_makeScenarioSimilar_works cw sw
  = prop_makeSimilar_works (scenarioDiff cw sw) makeScenarioSimilar

test :: Test
test = testGroup "ALife.Creatur.Wain.ScenarioQC"
  [
    testProperty "prop_serialize_round_trippable - Scenario"
      (prop_serialize_round_trippable :: Scenario -> Property),
    testProperty "prop_genetic_round_trippable - Scenario"
      (prop_genetic_round_trippable (==) :: Scenario -> Property),
    testProperty "prop_diploid_identity - Scenario"
      (prop_diploid_identity (==) :: Scenario -> Property),
    testProperty "prop_diploid_expressable - Scenario"
      (prop_diploid_expressable :: Scenario -> Scenario -> Property),
    testProperty "prop_diploid_readable - Scenario"
      (prop_diploid_readable :: Scenario -> Scenario -> Property),
    -- testProperty "prop_scenarioDiff_can_be_1"
    --   prop_scenarioDiff_can_be_1,
    testProperty "prop_scenarioDiff_can_be_0"
      prop_scenarioDiff_can_be_0,
    testProperty "prop_scenarioDiff_in_range"
      prop_scenarioDiff_in_range,
    testProperty "prop_makeScenarioSimilar_works"
      prop_makeScenarioSimilar_works
  ]
