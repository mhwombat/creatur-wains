------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ClassifierQC
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.ClassifierQC
  (
    test,
    equiv
  ) where

import ALife.Creatur.Wain.Classifier
import ALife.Creatur.Wain.GeneticSOMQC (equiv)
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity, TestPattern)
import Control.Applicative ((<$>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

data Counter = Counter {getInt :: Int} deriving Show

sizedArbCounter :: Int -> Gen Counter
sizedArbCounter n = Counter <$> oneof [choose (0,n*1000), return 0]
-- The small numbers help to avoid overflow, which shouldn't be a
-- problem in the real application.
-- Without the "return 0" part, we wouldn't be likely to get enough
-- zeros for a proper test (e.g., where all of the counters are zero).

instance Arbitrary Counter where
  arbitrary = sized sizedArbCounter

-- The < check ensures we don't overflow, which shouldn't happen in
-- the real application.
prop_conflation'_within_bounds :: [Counter] -> Property
prop_conflation'_within_bounds xs = property $ 0 <= y && y <= 1 + tad
  where y = conflation' xs'
        xs' = map getInt xs
        tad = 1e-12

prop_conflation_within_bounds :: Classifier TestPattern -> Property
prop_conflation_within_bounds c = property $ 0 <= cf && cf <= 1
  where cf = conflation c

test :: Test
test = testGroup "ALife.Creatur.Wain.ClassifierQC"
  [
    testProperty "prop_serialize_round_trippable - Classifier"
      (prop_serialize_round_trippable :: Classifier TestPattern -> Property),
    testProperty "prop_genetic_round_trippable - Classifier"
      (prop_genetic_round_trippable equiv :: Classifier TestPattern -> Property),
    testProperty "prop_diploid_identity - Classifier"
      (prop_diploid_identity equiv :: Classifier TestPattern -> Property),
    testProperty "prop_conflation'_within_bounds"
      prop_conflation'_within_bounds,
    testProperty "prop_conflation_within_bounds"
      prop_conflation_within_bounds
  ]
