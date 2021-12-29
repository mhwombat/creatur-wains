------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PatternQC
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
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.PatternQC
  (
    test,
    TestPattern(..),
    randomTestPattern,
    prop_makeSimilar_works,
    prop_diff_can_be_0,
    prop_diff_can_be_1,
    prop_diff_is_symmetric
  ) where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import qualified ALife.Creatur.Gene.Test                 as GT
import qualified ALife.Creatur.Genetics.BRGCWord8        as W8
import           ALife.Creatur.Genetics.Diploid          (Diploid)
import           ALife.Creatur.Wain.Pattern              (Pattern, diff,
                                                          makeSimilar)
import           ALife.Creatur.Wain.Pretty               (Pretty)
-- import           ALife.Creatur.Wain.Statistics           (Statistical, stats)
import           Control.DeepSeq                         (NFData)
import           Control.Monad.Random                    (Rand, RandomGen,
                                                          getRandom)
import qualified Data.Datamining.Pattern.Numeric         as N
import           Data.Serialize                          (Serialize)
import           Data.Word                               (Word8)
import           GHC.Generics                            (Generic)
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)
import           Test.QuickCheck                         (Arbitrary, arbitrary)

-- | A simple pattern that is useful for testing.
newtype TestPattern = TestPattern Word8
  deriving (Show, Read, Eq, Generic, Serialize, W8.Genetic, Diploid,
            NFData, Ord, Pretty, Bounded)

instance Arbitrary TestPattern where
  arbitrary = TestPattern <$> arbitrary

-- instance Statistical TestPattern where
--   stats (TestPattern x) = [iStat "" x]

instance Pattern TestPattern where
  diff (TestPattern x) (TestPattern y)
    = UI.narrow $ abs (x' - y') / range
    where x' = fromIntegral x :: Double
          y' = fromIntegral y :: Double
          range = fromIntegral (maxBound :: Word8) :: Double
  makeSimilar (TestPattern target) r (TestPattern x)
    = TestPattern . round $ N.makeSimilar target' r' x'
    where target' = fromIntegral target :: Double
          r' = UI.wide r
          x' = fromIntegral x :: Double

-- | Random pattern generator.
randomTestPattern :: RandomGen r => Rand r TestPattern
randomTestPattern = TestPattern <$> getRandom

prop_diff_can_be_0 :: Pattern a => a -> Bool
prop_diff_can_be_0 x = diff x x == 0

prop_diff_can_be_1 :: (Pattern a, Bounded a) => a -> Bool
prop_diff_can_be_1 dummy
  = diff (minBound `asTypeOf` dummy) (maxBound `asTypeOf` dummy) == 1

prop_diff_is_symmetric :: Pattern a => a -> a -> Bool
prop_diff_is_symmetric x y = diff x y == diff y x

-- | Verify that `makeSimilar a b` returns a value that is no further
--   away from `b` than `a` was.
prop_makeSimilar_works
  :: Pattern a => a -> UI.UIDouble -> a -> Bool
prop_makeSimilar_works x r y = diffAfter <= diffBefore
  where diffBefore = diff x y
        y' = makeSimilar x r y
        diffAfter = diff x y'

test :: Test
test = testGroup "ALife.Creatur.Gene.PatternQC"
  [
    testProperty "prop_serialize_round_trippable - TestPattern"
      (GT.prop_serialize_round_trippable :: TestPattern -> Bool),
    testProperty "prop_genetic_round_trippable - TestPattern"
      (GT.prop_genetic_round_trippable (==) :: TestPattern -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - TestPattern"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestPattern -> Property),
    testProperty "prop_diploid_identity - TestPattern"
      (GT.prop_diploid_identity (==) :: TestPattern -> Bool),
    testProperty "prop_show_read_round_trippable - TestPattern"
      (GT.prop_show_read_round_trippable (==) :: TestPattern -> Bool),
    testProperty "prop_diploid_expressable - TestPattern"
      (GT.prop_diploid_expressable :: TestPattern -> TestPattern -> Bool),
    testProperty "prop_diploid_readable - TestPattern"
      (GT.prop_diploid_readable :: TestPattern -> TestPattern -> Bool),

    testProperty "prop_diff_can_be_0 - TestPattern"
      (prop_diff_can_be_0 :: TestPattern -> Bool),
    testProperty "prop_diff_can_be_1 - TestPattern"
      (prop_diff_can_be_1 :: TestPattern -> Bool),
    testProperty "prop_diff_is_symmetric - TestPattern"
      (prop_diff_is_symmetric :: TestPattern -> TestPattern -> Bool),
    testProperty "prop_makeSimilar_works - TestPattern"
      (prop_makeSimilar_works :: TestPattern -> UI.UIDouble -> TestPattern -> Bool)
  ]
