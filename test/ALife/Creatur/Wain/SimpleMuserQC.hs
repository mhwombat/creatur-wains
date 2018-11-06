------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SimpleMuserQC
-- Copyright   :  (c) Amy de Buitléir 2013-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.SimpleMuserQC
  (
    test,
    equivMuser,
    sizedArbMuser
  ) where

import           ALife.Creatur.Wain.PlusMinusOneQC
    (equivPM1Double)
import           ALife.Creatur.Wain.ResponseQC
    (TestAction)
import           ALife.Creatur.Wain.SimpleMuser
import           ALife.Creatur.Wain.TestUtils
import           Test.Framework
    (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2
    (testProperty)
import           Test.QuickCheck

sizedArbMuser :: Int -> Gen (SimpleMuser TestAction)
sizedArbMuser n = do
  o <- vectorOf 4 arbitrary
  d <- choose (1, min 3 (fromIntegral n + 1))
  let (Right m) = makeMuser o d
  return m

instance Arbitrary (SimpleMuser TestAction) where
  arbitrary = sized sizedArbMuser

equivMuser :: SimpleMuser TestAction -> SimpleMuser TestAction -> Bool
equivMuser x y
  = and
      (zipWith equivPM1Double (_defaultOutcomes x) (_defaultOutcomes y))
      && _depth x == _depth y

test :: Test
test = testGroup "ALife.Creatur.Wain.SimpleMuserQC"
  [
    testProperty "prop_serialize_round_trippable - SimpleMuser"
      (prop_serialize_round_trippable :: SimpleMuser TestAction -> Property),
    testProperty "prop_genetic_round_trippable - SimpleMuser"
      (prop_genetic_round_trippable equivMuser :: SimpleMuser TestAction -> Property),
    testProperty "prop_diploid_identity - SimpleMuser"
      (prop_diploid_identity (==) :: SimpleMuser TestAction -> Property),
    testProperty "prop_diploid_expressable - SimpleMuser"
      (prop_diploid_expressable
       :: SimpleMuser TestAction -> SimpleMuser TestAction -> Property),
    testProperty "prop_diploid_readable - SimpleMuser"
      (prop_diploid_readable :: SimpleMuser TestAction -> SimpleMuser TestAction -> Property)
  ]
