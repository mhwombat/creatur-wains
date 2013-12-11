------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ResponseQC
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module ALife.Creatur.Wain.ResponseQC
  (
    test,
    TestAction
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Response
import ALife.Creatur.Wain.ScenarioQC ()
import ALife.Creatur.Wain.ConditionQC ()
import ALife.Creatur.Wain.UnitIntervalQC ()
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity)
import Control.Applicative
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

data TestAction = Smile | Frown deriving (Show, Eq, Generic, Enum, Bounded)

instance Serialize TestAction
instance Genetic TestAction
instance Diploid TestAction

instance Arbitrary TestAction where
  arbitrary = elements [minBound .. maxBound]

type TestResponse = Response TestAction

instance Arbitrary TestResponse where
  arbitrary = do
    r <- arbitrary
    Response <$> arbitrary <*> arbitrary <*> pure (Just r)

test :: Test
test = testGroup "ALife.Creatur.Wain.ResponseQC"
  [
    testProperty "prop_serialize_round_trippable - Response"
      (prop_serialize_round_trippable :: TestResponse -> Property),
    testProperty "prop_genetic_round_trippable - Response"
      (prop_genetic_round_trippable (==) :: TestResponse -> Property),
    testProperty "prop_diploid_identity - Response"
      (prop_diploid_identity (==) :: TestResponse -> Property)
  ]
