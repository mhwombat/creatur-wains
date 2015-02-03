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
import ALife.Creatur.Wain.Util (unitInterval)
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.UnitInterval (UIDouble(..))
import ALife.Creatur.Wain.UnitIntervalQC ()
import Control.Applicative
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary Condition where
  arbitrary = Condition <$> arb8BitDouble unitInterval
                <*> arb8BitDouble unitInterval <*> arbitrary

prop_happiness_in_range :: UIDouble -> Condition -> Property
prop_happiness_in_range (UIDouble e) c = property $ 0 <= x && x <= 1
  where x = happiness e c

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
    testProperty "prop_happiness_in_range" prop_happiness_in_range
  ]
