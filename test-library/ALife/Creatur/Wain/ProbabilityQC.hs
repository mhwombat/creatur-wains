------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ProbabilityQC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.ProbabilityQC
  (
    test
  ) where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import           ALife.Creatur.Wain.GeneticSOM           (Label)
import           ALife.Creatur.Wain.ProbabilityInternal
import           Data.List                               (nub)
import           Data.Word                               (Word32)
import qualified Numeric.ApproxEq                        as N
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)
import           Test.QuickCheck

newtype PermuteTestData = PermuteTestData [[Char]] deriving (Eq, Show)

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

prop_number_of_permutations_is_correct :: PermuteTestData -> Bool
prop_number_of_permutations_is_correct (PermuteTestData xss)
  = length ys == product (map length xss)
  where ys = permute xss

prop_permutations_are_distinct :: PermuteTestData -> Bool
prop_permutations_are_distinct (PermuteTestData xss)
  = length ys == length (nub ys)
  where ys = permute xss

prop_normalise_works :: [Double] -> Property
prop_normalise_works ps = not (null ps) ==>
  N.within 10000 (sum . normalise $ ps) 1

newtype TestSignatures = TestSignatures [[(Label, UI.Double)]]
  deriving (Eq, Show)

sizedArbTestSignatures :: Int -> Gen TestSignatures
sizedArbTestSignatures n = do
  k <- min 3 <$> choose (1, n+1)
  let m = min 3 $ n + 2 - k
  TestSignatures <$> vectorOf k (vectorOf m arbitrary)

instance Arbitrary TestSignatures where
  arbitrary = sized sizedArbTestSignatures

prop_hypothesis_probabilities_eq_1 :: Word32 -> TestSignatures -> Property
prop_hypothesis_probabilities_eq_1 s (TestSignatures lds) = s >= 1 ==>
  N.within 100 (sum . map (UI.wide . snd) $ hps) 1
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
