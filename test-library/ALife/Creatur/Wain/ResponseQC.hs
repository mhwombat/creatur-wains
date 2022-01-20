------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ResponseQC
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
module ALife.Creatur.Wain.ResponseQC
  (
    test,
    TestResponse,
    TestAction(..),
    TestResponseAdjuster(..)
  ) where

import qualified ALife.Creatur.Gene.AdjusterTest         as AT
import qualified ALife.Creatur.Gene.Numeric.PlusMinusOne as PM1
import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import qualified ALife.Creatur.Gene.Test                 as GT
import qualified ALife.Creatur.Genetics.BRGCWord8        as G
import           ALife.Creatur.Genetics.Diploid          (Diploid)
import           ALife.Creatur.Wain.GeneticSOM           (Label)
import           ALife.Creatur.Wain.LearningParams       (LearningParams,
                                                          toLearningFunction)
import           ALife.Creatur.Wain.LearningParamsQC     ()
import           ALife.Creatur.Wain.Pretty               (Pretty)
import           ALife.Creatur.Wain.Report               (Report, report)
import           ALife.Creatur.Wain.ResponseInternal
import           ALife.Creatur.Wain.Statistics           (Statistical (..))
import           Control.DeepSeq                         (NFData)
import           Data.Datamining.Clustering.SGM4         (Adjuster, MetricType,
                                                          PatternType, TimeType,
                                                          difference,
                                                          learningRate,
                                                          makeSimilar)
import qualified Data.Datamining.Pattern.List            as L
import           Data.Serialize                          (Serialize)
import           Data.Word                               (Word32)
import           GHC.Generics                            (Generic)
import           System.Random                           (Random, random,
                                                          randomR)
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)
import           Test.QuickCheck.Counterexamples         hiding (labels)

data TestAction = Walk | Run | Jump | Skip | Crawl
  deriving ( Show, Pretty, Read, Eq, Ord, Generic, Enum, Bounded,
             Serialize, G.Genetic, Diploid, NFData )

instance Arbitrary TestAction where
  arbitrary = elements [minBound .. maxBound]

instance Statistical TestAction where
  stats _ = []

type TestResponse = Response TestAction

sizedArbTestResponse :: Int -> Gen TestResponse
sizedArbTestResponse = sizedArbResponse arbitrary

instance Arbitrary TestResponse where
  arbitrary = sized sizedArbTestResponse

-- | This is used for testing random SOM generation
instance Random TestAction where
  randomR (a,b) g = (toEnum n, g')
    where (n, g') = randomR (fromEnum a, fromEnum b) g
  random = randomR (minBound,maxBound)

prop_labelSimilarity_can_be_1 :: [Label] -> Bool
prop_labelSimilarity_can_be_1 xs = abs (x - 1) < 1e-8
  where x = labelSimilarity xs xs

prop_labelSimilarity_can_be_0 :: [Label] -> Property
prop_labelSimilarity_can_be_0 xs
  = (not . null $ xs) ==> abs x < 1e-8
  where x = labelSimilarity xs ys
        ys = map (+255) xs

newtype TestResponseAdjuster = TestResponseAdjuster LearningParams
  deriving (Eq, Show, Read, Generic, Serialize, G.Genetic, Diploid, NFData)

instance Adjuster TestResponseAdjuster where
  type TimeType TestResponseAdjuster = Word32
  type MetricType TestResponseAdjuster = UI.Double
  type PatternType TestResponseAdjuster = TestResponse
  learningRate (TestResponseAdjuster l) = toLearningFunction l
  difference _ x y =
    if action x == action y
      then 1 - labelSimilarity (labels x) (labels y)
      else 1
  makeSimilar _ target r x =
      if action target == action x
         then Response s a o
         else x
      where s = labels x -- never change this
            a = action x -- never change this
            o = L.makeSimilar PM1.makeDoubleSimilar (outcomes target) r (outcomes x)

instance Statistical TestResponseAdjuster where
  stats (TestResponseAdjuster l) = stats l

instance Report TestResponseAdjuster where
  report (TestResponseAdjuster l) = report l

instance Arbitrary TestResponseAdjuster where
  arbitrary = TestResponseAdjuster <$> arbitrary


test :: Test
test = testGroup "ALife.Creatur.Wain.ResponseQC"
  [
    testProperty "prop_serialize_round_trippable - TestResponse"
      (GT.prop_serialize_round_trippable :: TestResponse -> Bool),
    testProperty "prop_genetic_round_trippable - TestResponse"
      (GT.prop_genetic_round_trippable (==)
       :: TestResponse -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - TestResponse"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestResponse -> Bool),
    testProperty "prop_diploid_identity - TestResponse"
      (GT.prop_diploid_identity (==) :: TestResponse -> Bool),
    testProperty "prop_show_read_round_trippable - TestResponse"
      (GT.prop_show_read_round_trippable (==) :: TestResponse -> Bool),
    testProperty "prop_diploid_expressable - TestResponse"
      (GT.prop_diploid_expressable
       :: TestResponse -> TestResponse -> Bool),
    testProperty "prop_diploid_readable - TestResponse"
      (GT.prop_diploid_readable :: TestResponse -> TestResponse -> Bool),

    testProperty "prop_labelSimilarity_can_be_1"
      prop_labelSimilarity_can_be_1,
    testProperty "prop_labelSimilarity_can_be_0"
      prop_labelSimilarity_can_be_0,

    testProperty "prop_serialize_round_trippable - TestResponseAdjuster"
      (GT.prop_serialize_round_trippable :: TestResponseAdjuster -> Bool),
    testProperty "prop_genetic_round_trippable - TestResponseAdjuster"
      (GT.prop_genetic_round_trippable (==) :: TestResponseAdjuster -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - TestResponseAdjuster"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestResponseAdjuster -> Bool),
    testProperty "prop_diploid_identity - TestResponseAdjuster"
      (GT.prop_diploid_identity (==) :: TestResponseAdjuster -> Bool),
    testProperty "prop_show_read_round_trippable - TestResponseAdjuster"
      (GT.prop_show_read_round_trippable (==) :: TestResponseAdjuster -> Bool),
    testProperty "prop_diploid_expressable - TestResponseAdjuster"
      (GT.prop_diploid_expressable
       :: TestResponseAdjuster -> TestResponseAdjuster -> Bool),
    testProperty "prop_diploid_readable - TestResponseAdjuster"
      (GT.prop_diploid_readable
       :: TestResponseAdjuster -> TestResponseAdjuster -> Bool),


    testProperty "prop_diff_can_be_0 - TestResponseAdjuster"
      (AT.prop_diff_can_be_0 :: TestResponseAdjuster -> TestResponse -> Bool),
    -- testProperty "prop_diff_can_be_1 - TestResponseAdjuster"
    --   (AT.prop_diff_can_be_1 :: TestResponseAdjuster -> TestResponse -> Bool),
    testProperty "prop_diff_is_symmetric - TestResponseAdjuster"
      (AT.prop_diff_is_symmetric :: TestResponseAdjuster -> TestResponse -> TestResponse -> Bool),
    testProperty "prop_makeSimilar_improves_similarity - TestResponseAdjuster"
      (AT.prop_makeSimilar_improves_similarity :: TestResponseAdjuster -> TestResponse -> UI.Double -> TestResponse -> Bool)
    -- testProperty "prop_zero_adjustment_makes_no_change - TestResponseAdjuster"
    --   (AT.prop_zero_adjustment_makes_no_change (==) :: TestResponseAdjuster -> TestResponse -> TestResponse -> Bool),
    -- testProperty "prop_full_adjustment_gives_perfect_match - TestResponseAdjuster"
    --   (AT.prop_full_adjustment_gives_perfect_match (==) :: TestResponseAdjuster -> TestResponse -> TestResponse -> Bool)
  ]
