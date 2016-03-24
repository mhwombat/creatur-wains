------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UtilQC
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
module ALife.Creatur.Wain.UtilQC
  (
    test
  ) where

import ALife.Creatur.Wain.Util
import Data.Word (Word8)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

prop_scaleToWord8_min :: (Double, Double) -> Property
prop_scaleToWord8_min (a, b) = property $ scaleToWord8 (a, b) a == 0

prop_scaleToWord8_max :: (Double, Double) -> Property
prop_scaleToWord8_max (a, b) = b > a ==> scaleToWord8 (a, b) b == 255

prop_scaleFromWord8_min :: (Double, Double) -> Property
prop_scaleFromWord8_min (a, b) = property $ scaleFromWord8 (a, b) 0 == a

prop_scaleFromWord8_max :: (Double, Double) -> Property
prop_scaleFromWord8_max (a, b) = b > a ==>
  abs ((scaleFromWord8 (a, b) 255) - b) < 1e-10

prop_scaleFromWord8_round_trippable :: (Double, Double) -> Word8 -> Property
prop_scaleFromWord8_round_trippable (a, b) x = b > a ==> x' == x
  where x' = scaleToWord8 (a, b) . scaleFromWord8 (a, b) $ x

-- prop_scaleFromWord64_round_trippable :: (Double, Double) -> Word64 -> Property
-- prop_scaleFromWord64_round_trippable (a, b) x = b > a ==> x' == x
--   where x' = scaleToWord64 (a, b) . scaleFromWord64 (a, b) $ 

test :: Test
test = testGroup "ALife.Creatur.Wain.UtilQC"
  [
    testProperty "prop_scaleToWord8_min"
      prop_scaleToWord8_min,
    testProperty "prop_scaleToWord8_max"
      prop_scaleToWord8_max,
    testProperty "prop_scaleFromWord8_min"
      prop_scaleFromWord8_min,
    testProperty "prop_scaleFromWord8_max"
      prop_scaleFromWord8_max,
    testProperty "prop_scaleFromWord8_round_trippable"
      prop_scaleFromWord8_round_trippable
    -- testProperty "prop_scaleFromWord64_round_trippable"
    --   prop_scaleFromWord64_round_trippable 
 ]
