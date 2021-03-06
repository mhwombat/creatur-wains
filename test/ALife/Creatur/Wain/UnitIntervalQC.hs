------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UnitIntervalQC
-- Copyright   :  (c) Amy de Buitléir 2013-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.UnitIntervalQC
  (
    test
  ) where

import ALife.Creatur.Wain.TestUtils
    ( prop_diploid_expressable
    , prop_diploid_identity
    , prop_diploid_readable
    , prop_genetic_round_trippable
    , prop_makeSimilar_works
    , prop_serialize_round_trippable
    , prop_show_read_round_trippable
    )
import ALife.Creatur.Wain.UnitInterval
import Control.DeepSeq
    (deepseq)
import Test.Framework
    (Test, testGroup)
import Test.Framework.Providers.QuickCheck2
    (testProperty)
import Test.QuickCheck

instance Arbitrary UIDouble where
  arbitrary = doubleToUI <$> choose interval

prop_max_uiDiff_is_1 :: Bool
prop_max_uiDiff_is_1 = uiDiff (doubleToUI 0) (doubleToUI 1) == 1

prop_min_uiDiff_is_0 :: Bool
prop_min_uiDiff_is_0 = uiDiff (doubleToUI 1) (doubleToUI 1) == 0

prop_uiDiff_in_range :: UIDouble -> UIDouble -> Property
prop_uiDiff_in_range x y = property $ deepseq (uiDiff x y) True

prop_uiDiff_is_symmetric :: UIDouble -> UIDouble -> Property
prop_uiDiff_is_symmetric x y = property $ uiDiff x y == uiDiff y x

prop_max_uiVectorDiff_is_1 :: Int -> Property
prop_max_uiVectorDiff_is_1 n = n > 0 ==> uiVectorDiff ones zeroes == 1
  where ones = map doubleToUI . replicate n $ 1
        zeroes = map doubleToUI . replicate n $ 0

prop_min_uiVectorDiff_is_0 :: Int -> Property
prop_min_uiVectorDiff_is_0 n = property $ uiVectorDiff ones ones == 0
  where ones = map doubleToUI . replicate n $ 1

prop_uiVectorDiff_in_range :: [UIDouble] -> [UIDouble] -> Property
prop_uiVectorDiff_in_range x y = property $ 0 <= diff && diff <= 1
  where diff = uiVectorDiff x y

prop_uiVectorDiff_is_symmetric :: [UIDouble] -> [UIDouble] -> Property
prop_uiVectorDiff_is_symmetric x y = property $
  uiVectorDiff x y == uiVectorDiff y x

prop_normalise_works :: [UIDouble] -> Property
prop_normalise_works xs
  = not (null ys) ==> sum ys <= 1 && (sum ys - 1) < 1e-10
  where ys = map uiToDouble $ normalise xs

test :: Test
test = testGroup "ALife.Creatur.Wain.UnitIntervalQC"
  [
    testProperty "prop_serialize_round_trippable - UIDouble"
      (prop_serialize_round_trippable :: UIDouble -> Property),
    testProperty "prop_genetic_round_trippable - UIDouble"
      (prop_genetic_round_trippable (==) :: UIDouble -> Property),
    -- testProperty "prop_genetic_round_trippable2 - UIDouble"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> UIDouble -> Property),
    testProperty "prop_diploid_identity - UIDouble"
      (prop_diploid_identity (==) :: UIDouble -> Property),
    testProperty "prop_show_read_round_trippable - UIDouble"
      (prop_show_read_round_trippable (==) :: UIDouble -> Property),
    testProperty "prop_diploid_expressable - UIDouble"
      (prop_diploid_expressable :: UIDouble -> UIDouble -> Property),
    testProperty "prop_diploid_readable - UIDouble"
      (prop_diploid_readable :: UIDouble -> UIDouble -> Property),

    testProperty "prop_serialize_round_trippable - [UIDouble]"
      (prop_serialize_round_trippable :: [UIDouble] -> Property),
    testProperty "prop_genetic_round_trippable - [UIDouble]"
      (prop_genetic_round_trippable (==) :: [UIDouble] -> Property),
    -- testProperty "prop_genetic_round_trippable2 - [UIDouble]"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> [UIDouble] -> Property),
    testProperty "prop_diploid_identity - [UIDouble]"
      (prop_diploid_identity (==) :: [UIDouble] -> Property),
    testProperty "prop_show_read_round_trippable - [UIDouble]"
      (prop_show_read_round_trippable (==) :: [UIDouble] -> Property),
    testProperty "prop_diploid_expressable - [UIDouble]"
      (prop_diploid_expressable :: [UIDouble] -> [UIDouble] -> Property),
    testProperty "prop_diploid_readable - [UIDouble]"
      (prop_diploid_readable :: [UIDouble] -> [UIDouble] -> Property),

    testProperty "prop_max_uiDiff_is_1"
      prop_max_uiDiff_is_1,
    testProperty "prop_min_uiDiff_is_0"
      prop_min_uiDiff_is_0,
    testProperty "prop_uiDiff_in_range"
      prop_uiDiff_in_range,
    testProperty "prop_uiDiff_is_symmetric"
      prop_uiDiff_is_symmetric,
    testProperty "prop_makeSimilar_works - UIDouble"
      (prop_makeSimilar_works uiDiff adjustUIDouble),
    testProperty "prop_max_uiVectorDiff_is_1"
      prop_max_uiVectorDiff_is_1,
    testProperty "prop_min_uiVectorDiff_is_0"
      prop_min_uiVectorDiff_is_0,
    testProperty "prop_uiVectorDiff_in_range"
      prop_uiVectorDiff_in_range,
    testProperty "prop_uiVectorDiff_is_symmetric"
      prop_uiVectorDiff_is_symmetric,
    testProperty "prop_makeSimilar_works - UI Vector"
      (prop_makeSimilar_works uiVectorDiff adjustUIVector),
    testProperty "prop_normalise_works"
      prop_normalise_works
  ]
