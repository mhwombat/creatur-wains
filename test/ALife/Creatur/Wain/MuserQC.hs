------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.MuserQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.MuserQC
  (
    test,
    equivMuser,
    sizedArbMuser
  ) where

import ALife.Creatur.Wain.MuserInternal
import ALife.Creatur.Wain.PlusMinusOneQC (equivPM1Double)
import ALife.Creatur.Wain.TestUtils
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

sizedArbMuser :: Int -> Gen Muser
sizedArbMuser n = do
  o <- vectorOf 4 arbitrary
  d <- choose (1, min 3 (fromIntegral n + 1))
  let (Right m) = makeMuser o d
  return m

instance Arbitrary Muser where
  arbitrary = sized sizedArbMuser

equivMuser :: Muser -> Muser -> Bool
equivMuser x y
  = and
      (zipWith equivPM1Double (_defaultOutcomes x) (_defaultOutcomes y))
      && _depth x == _depth y

test :: Test
test = testGroup "ALife.Creatur.Wain.MuserQC"
  [
    testProperty "prop_serialize_round_trippable - Muser"
      (prop_serialize_round_trippable :: Muser -> Property),
    testProperty "prop_genetic_round_trippable - Muser"
      (prop_genetic_round_trippable equivMuser :: Muser -> Property),
    testProperty "prop_diploid_identity - Muser"
      (prop_diploid_identity (==) :: Muser -> Property),
    testProperty "prop_diploid_expressable - Muser"
      (prop_diploid_expressable
       :: Muser -> Muser -> Property),
    testProperty "prop_diploid_readable - Muser"
      (prop_diploid_readable :: Muser -> Muser -> Property)
  ]
