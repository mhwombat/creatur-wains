------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ScenarioQC
-- Copyright   :  (c) Amy de Buitléir 2013
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
import ALife.Creatur.Wain.UnitIntervalQC ()
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity)
import Control.Applicative
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary Scenario where
  arbitrary = Scenario <$> arbitrary <*> arbitrary <*> arbitrary

test :: Test
test = testGroup "ALife.Creatur.Wain.ScenarioQC"
  [
    testProperty "prop_serialize_round_trippable - Scenario"
      (prop_serialize_round_trippable :: Scenario -> Property),
    testProperty "prop_genetic_round_trippable - Scenario"
      (prop_genetic_round_trippable (==) :: Scenario -> Property),
    testProperty "prop_diploid_identity - Scenario"
      (prop_diploid_identity (==) :: Scenario -> Property)
  ]
