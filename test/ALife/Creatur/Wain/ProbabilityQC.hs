------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ProbabilityQC
-- Copyright   :  (c) Amy de Buitléir 2013-2019
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
module ALife.Creatur.Wain.ProbabilityQC
  (
    test
  ) where

import           ALife.Creatur.Wain.GeneticSOMInternal  (Difference, Label)
import           ALife.Creatur.Wain.ProbabilityInternal
import           ALife.Creatur.Wain.UnitInterval        (uiToDouble)
import           ALife.Creatur.Wain.UnitIntervalQC      ()
import           Data.List                              (nub)
import           Data.Word                              (Word64)
import qualified Numeric.ApproxEq                       as N
import           Test.Framework                         (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2   (testProperty)
import           Test.QuickCheck

data PermuteTestData = PermuteTestData [[Char]] deriving (Eq, Show)

sizedArbString :: Int -> Gen String
sizedArbString n = do
  k <- choose (1, n+1)
  nub <$> vectorOf k (elements ['a'..'z'])

sizedArbPermuteTestData :: Int -> Gen PermuteTestData
sizedArbPermuteTestData n = do
  k <- min 3 <$> choose (1, n+1)
  let m = min 3 $ n + 2 - k
  PermuteTestData <$> vectorOf k (sizedArbString m)

instance Arbitrary PermuteTestData where
  arbitrary = sized sizedArbPermuteTestData

prop_number_of_permutations_is_correct :: PermuteTestData -> Property
prop_number_of_permutations_is_correct (PermuteTestData xss)
  = property $ length ys == product (map length xss)
  where ys = permute xss

prop_permutations_are_distinct :: PermuteTestData -> Property
prop_permutations_are_distinct (PermuteTestData xss) = property $
  length ys == length (nub ys)
  where ys = permute xss

prop_normalise_works :: [Double] -> Property
prop_normalise_works ps = not (null ps) ==>
  N.within 10000 (sum . normalise $ ps) 1

data TestSignatures = TestSignatures [[(Label, Difference)]]
  deriving (Eq, Show)

sizedArbTestSignatures :: Int -> Gen TestSignatures
sizedArbTestSignatures n = do
  k <- min 3 <$> choose (1, n+1)
  let m = min 3 $ n + 2 - k
  TestSignatures <$> vectorOf k (vectorOf m arbitrary)

instance Arbitrary TestSignatures where
  arbitrary = sized sizedArbTestSignatures

prop_hypothesis_probabilities_eq_1 :: Word64 -> TestSignatures -> Property
prop_hypothesis_probabilities_eq_1 s (TestSignatures lds) = s >= 1 ==>
  N.within 100 (sum . map (uiToDouble . snd) $ hps) 1
  where hps = hypothesise s lds

test :: Test
test = testGroup "ALife.Creatur.Wain.ProbabilityQC"
  [
    testProperty "prop_number_of_permutations_is_correct"
      prop_number_of_permutations_is_correct,
    testProperty "prop_permutations_are_distinct"
      prop_permutations_are_distinct,
    testProperty "prop_normalise_works" prop_normalise_works,
    testProperty "prop_hypothesis_probabilities_eq_1"
      prop_hypothesis_probabilities_eq_1
  ]
