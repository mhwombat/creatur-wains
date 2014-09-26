------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ClassificationMetricsQC
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.ClassificationMetricsQC
  (
    test
  ) where

import ALife.Creatur.Wain.ClassificationMetrics
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

data TestData = TestData [(Char,Int)] Char deriving Show

sizedArbTestData :: Int -> Gen TestData
sizedArbTestData n = do
  ls <- vectorOf (n+1) arbitrary
  ks <- vectorOf (n+1) arbitrary :: Gen [Positive Int]
  let cs = zip ls (map getPositive ks)
  l <- elements ls
  return $ TestData cs l

instance Arbitrary TestData where
  arbitrary = sized sizedArbTestData

prop_discrimination_ge_0 :: TestData -> Property
prop_discrimination_ge_0 (TestData cs _) = property $ discrimination ns >= 0
  where ns = map snd cs

prop_novelty_ge_0 :: TestData -> Property
prop_novelty_ge_0 (TestData cs l) = property $ novelty l cs >= 0

prop_novelty_le_1 :: TestData -> Property
prop_novelty_le_1 (TestData cs l) = property $ novelty l cs <= 1

test :: Test
test = testGroup "ALife.Realtra.ClassificationQC"
  [
    testProperty "prop_discrimination_ge_0"
      prop_discrimination_ge_0,
    testProperty "prop_novelty_ge_0"
      prop_novelty_ge_0,
    testProperty "prop_novelty_le_1"
      prop_novelty_le_1
  ]
