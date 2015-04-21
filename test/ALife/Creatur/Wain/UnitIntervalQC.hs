------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UnitVectorQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.UnitIntervalQC
  (
    -- test,
    -- equiv,
    -- equiv2
  ) where

import ALife.Creatur.Wain.UnitInterval
import ALife.Creatur.Wain.Util (unitInterval)
-- import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
--   prop_genetic_round_trippable, prop_diploid_identity)
-- import Test.Framework (Test, testGroup)
-- import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif

instance Arbitrary UIDouble where
  arbitrary = doubleToUI <$> choose unitInterval

-- equiv :: UIDouble -> UIDouble -> Bool
-- equiv x y = (uiDiff x y) < (1/255)

-- equiv2 :: [UIDouble] -> [UIDouble] -> Bool
-- equiv2 x y = (uiVectorDiff x y) < (1/255)

-- prop_max_uiDiff_is_1 :: Property
-- prop_max_uiDiff_is_1 = property $ uiDiff (UIDouble 0) (UIDouble 1) == 1

-- prop_min_uiDiff_is_0 :: Property
-- prop_min_uiDiff_is_0 = property $ uiDiff (UIDouble 1) (UIDouble 1) == 0

-- prop_uiDiff_in_range :: UIDouble -> UIDouble -> Property
-- prop_uiDiff_in_range x y = property $ 0 <= diff && diff <= 1
--   where diff = uiDiff x y

-- prop_uiDiff_is_symmetric :: UIDouble -> UIDouble -> Property
-- prop_uiDiff_is_symmetric x y = property $ uiDiff x y == uiDiff y x

-- prop_max_uiVectorDiff_is_1 :: Int -> Property
-- prop_max_uiVectorDiff_is_1 n = n > 0 ==> uiVectorDiff ones zeroes == 1
--   where ones = map UIDouble . replicate n $ 1
--         zeroes = map UIDouble . replicate n $ 0

-- prop_max_uiVectorDiff_is_0 :: Int -> Property
-- prop_max_uiVectorDiff_is_0 n = property $ uiVectorDiff ones ones == 0
--   where ones = map UIDouble . replicate n $ 1

-- prop_uiVectorDiff_in_range :: [UIDouble] -> [UIDouble] -> Property
-- prop_uiVectorDiff_in_range x y = property $ 0 <= diff && diff <= 1
--   where diff = uiVectorDiff x y

-- prop_uiVectorDiff_is_symmetric :: [UIDouble] -> [UIDouble] -> Property
-- prop_uiVectorDiff_is_symmetric x y = property $
--   uiVectorDiff x y == uiVectorDiff y x

-- test :: Test
-- test = testGroup "ALife.Creatur.Wain.UnitIntervalQC"
--   [
--     testProperty "prop_serialize_round_trippable - UIDouble"
--       (prop_serialize_round_trippable :: UIDouble -> Property),
--     testProperty "prop_genetic_round_trippable - UIDouble"
--       (prop_genetic_round_trippable equiv :: UIDouble -> Property),
--     testProperty "prop_diploid_identity - UIDouble"
--       (prop_diploid_identity (==) :: UIDouble -> Property),
--     testProperty "prop_serialize_round_trippable - [UIDouble]"
--       (prop_serialize_round_trippable :: [UIDouble] -> Property),
--     testProperty "prop_genetic_round_trippable - [UIDouble]"
--       (prop_genetic_round_trippable equiv2 :: [UIDouble] -> Property),
--     testProperty "prop_diploid_identity - [UIDouble]"
--       (prop_diploid_identity (==) :: [UIDouble] -> Property),
--     testProperty "prop_max_uiDiff_is_1"
--       prop_max_uiDiff_is_1,
--     testProperty "prop_min_uiDiff_is_0"
--       prop_min_uiDiff_is_0,
--     testProperty "prop_uiDiff_in_range"
--       prop_uiDiff_in_range,
--     testProperty "prop_uiDiff_is_symmetric"
--       prop_uiDiff_is_symmetric,
--     testProperty "prop_max_uiVectorDiff_is_1"
--       prop_max_uiVectorDiff_is_1,
--     testProperty "prop_max_uiVectorDiff_is_0"
--       prop_max_uiVectorDiff_is_0,
--     testProperty "prop_uiVectorDiff_in_range"
--       prop_uiVectorDiff_in_range,
--     testProperty "prop_uiVectorDiff_is_symmetric"
--       prop_uiVectorDiff_is_symmetric
--   ]
