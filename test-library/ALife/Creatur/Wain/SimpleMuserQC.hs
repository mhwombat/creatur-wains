------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SimpleMuserQC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
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
    sizedArbMuser
  ) where

import qualified ALife.Creatur.Gene.Test as GT
import ALife.Creatur.Wain.ResponseQC
    (TestAction)
import ALife.Creatur.Wain.SimpleMuserInternal
import Test.Framework
    (Test, testGroup)
import Test.Framework.Providers.QuickCheck2
    (testProperty)
import Test.QuickCheck

sizedArbMuser :: Int -> Gen (SimpleMuser TestAction)
sizedArbMuser n = do
  o <- vectorOf 4 arbitrary
  d <- choose (1, min 3 (fromIntegral n + 1))
  let (Right m) = makeMuser o d
  return m

instance Arbitrary (SimpleMuser TestAction) where
  arbitrary = sized sizedArbMuser

test :: Test
test = testGroup "ALife.Creatur.Wain.SimpleMuserQC"
  [
    testProperty "prop_serialize_round_trippable - SimpleMuser"
      (GT.prop_serialize_round_trippable :: SimpleMuser TestAction -> Property),
    testProperty "prop_genetic_round_trippable - SimpleMuser"
      (GT.prop_genetic_round_trippable (==) :: SimpleMuser TestAction -> Property),
    -- testProperty "prop_genetic_round_trippable2 - SimpleMuser"
    --   (GT.prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> SimpleMuser TestAction -> Property),
    testProperty "prop_diploid_identity - SimpleMuser"
      (GT.prop_diploid_identity (==) :: SimpleMuser TestAction -> Property),
    testProperty "prop_show_read_round_trippable - SimpleMuser"
      (GT.prop_show_read_round_trippable (==) :: SimpleMuser TestAction -> Property),
    testProperty "prop_diploid_expressable - SimpleMuser"
      (GT.prop_diploid_expressable :: SimpleMuser TestAction -> SimpleMuser TestAction -> Property),
    testProperty "prop_diploid_readable - SimpleMuser"
      (GT.prop_diploid_readable :: SimpleMuser TestAction -> SimpleMuser TestAction -> Property)
  ]
