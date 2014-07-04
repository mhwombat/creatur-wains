------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UnitVectorQC
-- Copyright   :  (c) Amy de Buitléir 2013-2014
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
    test,
    equiv,
    equiv2
  ) where

import ALife.Creatur.Wain.UnitInterval
import ALife.Creatur.Wain.Util (unitInterval)
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity)
import Control.Applicative ((<$>))
import Data.Datamining.Pattern (difference)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary UIDouble where
  arbitrary = doubleToUI <$> choose unitInterval

equiv :: UIDouble -> UIDouble -> Bool
equiv x y = abs (uiToDouble x - uiToDouble y) < (1/255)

equiv2 :: [UIDouble] -> [UIDouble] -> Bool
equiv2 x y = difference x y < (1/255)


prop_vector_difference_in_range :: [UIDouble] -> [UIDouble] -> Property
prop_vector_difference_in_range x y = property $ 0 <= diff && diff <= 1
  where diff = difference x y

test :: Test
test = testGroup "ALife.Creatur.Wain.UnitIntervalQC"
  [
    testProperty "prop_serialize_round_trippable - UIDouble"
      (prop_serialize_round_trippable :: UIDouble -> Property),
    testProperty "prop_genetic_round_trippable - UIDouble"
      (prop_genetic_round_trippable equiv :: UIDouble -> Property),
    testProperty "prop_diploid_identity - UIDouble"
      (prop_diploid_identity (==) :: UIDouble -> Property),
    testProperty "prop_serialize_round_trippable - [UIDouble]"
      (prop_serialize_round_trippable :: [UIDouble] -> Property),
    testProperty "prop_genetic_round_trippable - [UIDouble]"
      (prop_genetic_round_trippable equiv2 :: [UIDouble] -> Property),
    testProperty "prop_diploid_identity - [UIDouble]"
      (prop_diploid_identity (==) :: [UIDouble] -> Property),
    testProperty "prop_vector_difference_in_range"
      prop_vector_difference_in_range
  ]
