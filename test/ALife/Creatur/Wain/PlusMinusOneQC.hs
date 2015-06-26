------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PlusMinusOneQC
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
module ALife.Creatur.Wain.PlusMinusOneQC
  (
    test,
    equivPM1Double,
    equivPM1DoubleVector,
    negligible
  ) where

import ALife.Creatur.Wain.PlusMinusOne
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble)
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity)
import Control.DeepSeq (deepseq)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary PM1Double where
  arbitrary = doubleToPM1 <$> choose interval

negligible :: UIDouble -> Bool
negligible x = uiToDouble x < 1/255

equivPM1Double :: PM1Double -> PM1Double -> Bool
equivPM1Double x y = negligible $ pm1Diff x y

equivPM1DoubleVector :: [PM1Double] -> [PM1Double] -> Bool
equivPM1DoubleVector (x:xs) (y:ys)
  = equivPM1Double x y && equivPM1DoubleVector xs ys
equivPM1DoubleVector [] [] = True
equivPM1DoubleVector [] _ = False
equivPM1DoubleVector _ [] = False

prop_max_pm1Diff_is_1 :: Bool
prop_max_pm1Diff_is_1 = pm1Diff (doubleToPM1 (-1)) (doubleToPM1 1) == 1

prop_min_pm1Diff_is_0 :: Bool
prop_min_pm1Diff_is_0
  = pm1Diff (doubleToPM1 1) (doubleToPM1 1) == 0
      && pm1Diff (doubleToPM1 (-1)) (doubleToPM1 (-1)) == 0

prop_pm1Diff_in_range :: PM1Double -> PM1Double -> Property
prop_pm1Diff_in_range x y = property $ deepseq (pm1Diff x y) True

prop_pm1Diff_is_symmetric :: PM1Double -> PM1Double -> Property
prop_pm1Diff_is_symmetric x y = property $ pm1Diff x y == pm1Diff y x

prop_max_pm1VectorDiff_is_1 :: Int -> Property
prop_max_pm1VectorDiff_is_1 n = n > 0 ==>
  pm1VectorDiff ones minusOnes == 1
  where ones = map doubleToPM1 . replicate n $ 1
        minusOnes = map doubleToPM1 . replicate n $ -1

prop_max_pm1VectorDiff_is_0 :: Int -> Property
prop_max_pm1VectorDiff_is_0 n = property $ pm1VectorDiff ones ones == 0
  where ones = map doubleToPM1 . replicate n $ 1

prop_pm1VectorDiff_in_range :: [PM1Double] -> [PM1Double] -> Property
prop_pm1VectorDiff_in_range x y = property $ 0 <= diff && diff <= 1
  where diff = pm1VectorDiff x y

prop_pm1VectorDiff_is_symmetric :: [PM1Double] -> [PM1Double] -> Property
prop_pm1VectorDiff_is_symmetric x y = property $
  pm1VectorDiff x y == pm1VectorDiff y x

test :: Test
test = testGroup "ALife.Creatur.Wain.PlusMinusOneQC"
  [
    testProperty "prop_serialize_round_trippable - PM1Double"
      (prop_serialize_round_trippable :: PM1Double -> Property),
    testProperty "prop_genetic_round_trippable - PM1Double"
      (prop_genetic_round_trippable equivPM1Double :: PM1Double -> Property),
    testProperty "prop_diploid_identity - PM1Double"
      (prop_diploid_identity (==) :: PM1Double -> Property),
    testProperty "prop_serialize_round_trippable - [PM1Double]"
      (prop_serialize_round_trippable :: [PM1Double] -> Property),
    testProperty "prop_genetic_round_trippable - [PM1Double]"
      (prop_genetic_round_trippable equivPM1DoubleVector
        :: [PM1Double] -> Property),
    testProperty "prop_diploid_identity - [PM1Double]"
      (prop_diploid_identity (==) :: [PM1Double] -> Property),
    testProperty "prop_max_pm1Diff_is_1"
      prop_max_pm1Diff_is_1,
    testProperty "prop_min_pm1Diff_is_0"
      prop_min_pm1Diff_is_0,
    testProperty "prop_pm1Diff_in_range"
      prop_pm1Diff_in_range,
    testProperty "prop_pm1Diff_is_symmetric"
      prop_pm1Diff_is_symmetric,
    testProperty "prop_max_pm1VectorDiff_is_1"
      prop_max_pm1VectorDiff_is_1,
    testProperty "prop_max_pm1VectorDiff_is_0"
      prop_max_pm1VectorDiff_is_0,
    testProperty "prop_pm1VectorDiff_in_range"
      prop_pm1VectorDiff_in_range,
    testProperty "prop_pm1VectorDiff_is_symmetric"
      prop_pm1VectorDiff_is_symmetric
  ]
