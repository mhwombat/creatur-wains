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
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.PatternQC
  (
    test,
    TestPattern(..),
    TestPatternAdjuster(..),
    randomTestPattern
  ) where

import qualified ALife.Creatur.Gene.AdjusterTest         as AT
import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import qualified ALife.Creatur.Gene.Test                 as GT
import qualified ALife.Creatur.Genetics.BRGCWord8        as G
import qualified ALife.Creatur.Genetics.BRGCWord8        as W8
import           ALife.Creatur.Genetics.Diploid          (Diploid)
import           ALife.Creatur.Wain.Pretty               (Pretty)
-- import           ALife.Creatur.Wain.Statistics           (Statistical, stats)
import           ALife.Creatur.Wain.LearningParams       (LearningParams,
                                                          toLearningFunction)
import           ALife.Creatur.Wain.LearningParamsQC     ()
import           ALife.Creatur.Wain.Report               (Report, report)
import           ALife.Creatur.Wain.Statistics           (Statistical, stats)
import           Control.DeepSeq                         (NFData)
import           Control.Monad.Random                    (Rand, RandomGen,
                                                          getRandom)
import           Data.Datamining.Clustering.SGM4         (Adjuster, MetricType,
                                                          PatternType, TimeType,
                                                          difference,
                                                          learningRate,
                                                          makeSimilar)
import qualified Data.Datamining.Pattern.Numeric         as N
import           Data.Serialize                          (Serialize)
import           Data.Word                               (Word32, Word8)
import           GHC.Generics                            (Generic)
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)
import           Test.QuickCheck.Counterexamples         (Arbitrary, arbitrary)

-- | A simple pattern that is useful for testing.
newtype TestPattern = TestPattern Word8
  deriving (Show, Read, Eq, Generic, Serialize, W8.Genetic, Diploid,
            NFData, Ord, Pretty, Bounded)

instance Arbitrary TestPattern where
  arbitrary = TestPattern <$> arbitrary

-- instance Statistical TestPattern where
--   stats (TestPattern x) = [iStat "" x]


newtype TestPatternAdjuster = TestPatternAdjuster LearningParams
  deriving (Eq, Show, Read, Generic, Serialize, G.Genetic, Diploid, NFData)

instance Adjuster TestPatternAdjuster where
  type TimeType TestPatternAdjuster = Word32
  type MetricType TestPatternAdjuster = UI.Double
  type PatternType TestPatternAdjuster = TestPattern
  learningRate (TestPatternAdjuster l) = toLearningFunction l
  difference _ (TestPattern x) (TestPattern y)
    = UI.narrow $ abs (x' - y') / range
    where x' = fromIntegral x :: Double
          y' = fromIntegral y :: Double
          range = fromIntegral (maxBound :: Word8) :: Double
  makeSimilar _ (TestPattern target) r (TestPattern x)
    = TestPattern $ N.makeIntegralSimilar target r x

instance Statistical TestPatternAdjuster where
  stats (TestPatternAdjuster l) = stats l

instance Report TestPatternAdjuster where
  report (TestPatternAdjuster l) = report l

instance Arbitrary TestPatternAdjuster where
  arbitrary = TestPatternAdjuster <$> arbitrary

-- | Random pattern generator.
randomTestPattern :: RandomGen r => Rand r TestPattern
randomTestPattern = TestPattern <$> getRandom

-- prop_diff_can_be_0
--   :: (Adjuster a, Eq (MetricType a), Num (MetricType a))
--   => a -> PatternType a -> Bool
-- prop_diff_can_be_0 a x = difference a x x == 0

-- prop_diff_can_be_1
--   :: (Adjuster a, Bounded (PatternType a),
--      Eq (MetricType a), Num (MetricType a))
--   => a -> PatternType a -> Bool
-- prop_diff_can_be_1 a dummy
--   = difference a (minBound `asTypeOf` dummy) (maxBound `asTypeOf` dummy) == 1

-- prop_diff_is_symmetric
--   :: (Adjuster a, Eq (MetricType a))
--   => a -> PatternType a -> PatternType a -> Bool
-- prop_diff_is_symmetric a x y = difference a x y == difference a y x

-- -- | Verify that `makeSimilar a b` returns a value that is no further
-- --   away from `b` than `a` was.
-- prop_makeSimilar_works
--   :: (Adjuster a, Eq (MetricType a), Ord (MetricType a))
--   => a -> PatternType a -> MetricType a -> PatternType a -> Bool
-- prop_makeSimilar_works a x r y = diffAfter <= diffBefore
--   where diffBefore = difference a x y
--         y' = makeSimilar a x r y
--         diffAfter = difference a x y'


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

    testProperty "prop_serialize_round_trippable - TestPatternAdjuster"
      (GT.prop_serialize_round_trippable :: TestPatternAdjuster -> Bool),
    testProperty "prop_genetic_round_trippable - TestPatternAdjuster"
      (GT.prop_genetic_round_trippable (==) :: TestPatternAdjuster -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - TestPatternAdjuster"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestPatternAdjuster -> Bool),
    testProperty "prop_diploid_identity - TestPatternAdjuster"
      (GT.prop_diploid_identity (==) :: TestPatternAdjuster -> Bool),
    testProperty "prop_show_read_round_trippable - TestPatternAdjuster"
      (GT.prop_show_read_round_trippable (==) :: TestPatternAdjuster -> Bool),
    testProperty "prop_diploid_expressable - TestPatternAdjuster"
      (GT.prop_diploid_expressable
       :: TestPatternAdjuster -> TestPatternAdjuster -> Bool),
    testProperty "prop_diploid_readable - TestPatternAdjuster"
      (GT.prop_diploid_readable
       :: TestPatternAdjuster -> TestPatternAdjuster -> Bool),

    testProperty "prop_diff_can_be_0 - TestPatternAdjuster"
      (AT.prop_diff_can_be_0 :: TestPatternAdjuster -> TestPattern -> Bool),
    testProperty "prop_diff_can_be_1 - TestPatternAdjuster"
      (AT.prop_diff_can_be_1 :: TestPatternAdjuster -> TestPattern -> Bool),
    testProperty "prop_diff_is_symmetric - TestPatternAdjuster"
      (AT.prop_diff_is_symmetric :: TestPatternAdjuster -> TestPattern -> TestPattern -> Bool),
    testProperty "prop_makeSimilar_improves_similarity - TestPatternAdjuster"
      (AT.prop_makeSimilar_improves_similarity :: TestPatternAdjuster -> TestPattern -> UI.Double -> TestPattern -> Bool),
    testProperty "prop_zero_adjustment_makes_no_change - TestPatternAdjuster"
      (AT.prop_zero_adjustment_makes_no_change (==) :: TestPatternAdjuster -> TestPattern -> TestPattern -> Bool),
    testProperty "prop_full_adjustment_gives_perfect_match - TestPatternAdjuster"
      (AT.prop_full_adjustment_gives_perfect_match (==) :: TestPatternAdjuster -> TestPattern -> TestPattern -> Bool)
  ]
