------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ConditionQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.ConditionQC
  (
    test
  ) where

import ALife.Creatur.Wain.Condition
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.UnitIntervalQC ()
import ALife.Creatur.Wain.Util (unitInterval)
import ALife.Creatur.Wain.Weights (Weights, makeWeights)
import ALife.Creatur.Wain.WeightsQC ()
import ALife.Creatur.Wain.TestUtils
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary Condition where
  arbitrary = Condition <$> arb8BitDouble unitInterval
                <*> arb8BitDouble unitInterval <*> arbitrary

prop_happiness_can_be_1 :: Bool
prop_happiness_can_be_1 = abs (x - 1) < 1e-8
  where x = happiness w (Condition 1 0 1)
        w = makeWeights [1, 1, 1]

prop_happiness_can_be_0 :: Bool
prop_happiness_can_be_0 = abs (x - 0) < 1e-8
  where x = happiness w (Condition 0 1 0)
        w = makeWeights [1, 1, 1]

prop_happiness_in_range :: Weights -> Condition -> Property
prop_happiness_in_range w c = property $ 0 <= x && x <= 1
  where x = happiness w c

-- prop_conditionDiff_can_be_1 :: Weights -> Property
-- prop_conditionDiff_can_be_1 w = not (null ws) ==> abs (x - 1) < 1e-8
--   where x = conditionDiff w (Condition 0 0 0) (Condition 1 1 1)
--         ws = toDoubles w

prop_conditionDiff_can_be_0 :: Weights -> Condition -> Property
prop_conditionDiff_can_be_0 w c = property $ abs (x - 0) < 1e-8
  where x = conditionDiff w c c

prop_conditionDiff_in_range
  :: Weights -> Condition -> Condition -> Property
prop_conditionDiff_in_range w a b = property $ 0 <= x && x <= 1
  where x = conditionDiff w a b

prop_makeConditionSimilar_works
  :: Weights -> Condition -> UIDouble -> Condition -> Property
prop_makeConditionSimilar_works ws
  = prop_makeSimilar_works (conditionDiff ws) makeConditionSimilar

test :: Test
test = testGroup "ALife.Creatur.Wain.ConditionQC"
  [
    testProperty "prop_serialize_round_trippable - Condition"
      (prop_serialize_round_trippable :: Condition -> Property),
    testProperty "prop_genetic_round_trippable - Condition"
      (prop_genetic_round_trippable (==) :: Condition -> Property),
    testProperty "prop_diploid_identity - Condition"
      (prop_diploid_identity (==) :: Condition -> Property),
    testProperty "prop_diploid_expressable - Condition"
      (prop_diploid_expressable :: Condition -> Condition -> Property),
    testProperty "prop_diploid_readable - Condition"
      (prop_diploid_readable :: Condition -> Condition -> Property),
    testProperty "prop_happiness_can_be_1" prop_happiness_can_be_1,
    testProperty "prop_happiness_can_be_0" prop_happiness_can_be_0,
    testProperty "prop_happiness_in_range" prop_happiness_in_range,
    -- testProperty "prop_conditionDiff_can_be_1"
    --   prop_conditionDiff_can_be_1,
    testProperty "prop_conditionDiff_can_be_0"
      prop_conditionDiff_can_be_0,
    testProperty "prop_conditionDiff_in_range"
      prop_conditionDiff_in_range,
    testProperty "prop_makeConditionSimilar_works"
      prop_makeConditionSimilar_works
  ]
