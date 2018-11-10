------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ResponseQC
-- Copyright   :  (c) Amy de Buitléir 2013-2018
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
module ALife.Creatur.Wain.ResponseQC
  (
    test,
    TestResponse,
    TestAction(..),
    arbTestResponse
  ) where

import ALife.Creatur.Genetics.BRGCWord8
    (Genetic)
import ALife.Creatur.Genetics.Diploid
    (Diploid)
import ALife.Creatur.Wain.GeneticSOM
    (Label)
import ALife.Creatur.Wain.PlusMinusOneQC
    ()
import ALife.Creatur.Wain.Pretty
    (Pretty)
import ALife.Creatur.Wain.ResponseInternal
import ALife.Creatur.Wain.Statistics
    (Statistical (..))
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.UnitIntervalQC
    ()
import Control.DeepSeq
    (NFData)
import Data.Serialize
    (Serialize)
import GHC.Generics
    (Generic)
import System.Random
    (Random, random, randomR)
import Test.Framework
    (Test, testGroup)
import Test.Framework.Providers.QuickCheck2
    (testProperty)
import Test.QuickCheck

data TestAction = Walk | Run | Jump | Skip | Crawl
  deriving ( Show, Pretty, Read, Eq, Ord, Generic, Enum, Bounded,
             Serialize, Genetic, Diploid, NFData )

instance Arbitrary TestAction where
  arbitrary = elements [minBound .. maxBound]

instance Statistical TestAction where
  stats _ = []

type TestResponse = Response TestAction

sizedArbTestResponse :: Int -> Gen TestResponse
sizedArbTestResponse n = do
  nObjects <- choose (0, n)
  let nConditions = n - nObjects
  arbTestResponse nObjects nConditions

-- This method is used by other test classes to ensure that all of the
-- scenarios have the same number of objects and condition parameters.
arbTestResponse :: Int -> Int -> Gen TestResponse
arbTestResponse nObjects nConditions = do
  s <- vectorOf nObjects arbitrary
  a <- arbitrary
  o <- vectorOf nConditions arbitrary
  return $ Response s a o

instance Arbitrary TestResponse where
  arbitrary = sized sizedArbTestResponse

-- | This is used for testing random SOM generation
instance Random TestAction where
  randomR (a,b) g = (toEnum n, g')
    where (n, g') = randomR (fromEnum a, fromEnum b) g
  random = randomR (minBound,maxBound)

-- newtype TestOutcome = TestOutcome Double deriving (Show, Eq)

-- instance Random TestOutcome where
--   randomR (TestOutcome a, TestOutcome b) g = (TestOutcome x, g')
--     where (x, g') = randomR (a', b') g
--           (a', b') = intersection (-1,1) (a,b)
--   random g = (TestOutcome x, g')
--     where (x, g') = randomR (-1,1) g

-- instance Arbitrary TestOutcome where
--   arbitrary = TestOutcome <$> choose (-1,1)

prop_labelSimilarity_can_be_1 :: [Label] -> Property
prop_labelSimilarity_can_be_1 xs = property $ abs (x - 1) < 1e-8
  where x = labelSimilarity xs xs

prop_labelSimilarity_can_be_0 :: [Label] -> Property
prop_labelSimilarity_can_be_0 xs
  = (not . null $ xs) ==> abs (x - 0) < 1e-8
  where x = labelSimilarity xs ys
        ys = map (+255) xs

-- -- prop_responseDiff_can_be_1 :: Weights -> Property
-- -- prop_responseDiff_can_be_1 w = not (null ws) ==> abs (x - 1) < 1e-8
-- --   where x = responseDiff w (Response [0] [0] (Condition 0 0 0))
-- --               (Response [1] [1] (Condition 1 1 1))
-- --         ws = toDoubles w

-- prop_responseDiff_can_be_0 :: TestResponse -> Property
-- prop_responseDiff_can_be_0 r = property $ abs (x - 0) < 1e-8
--   where x = responseDiff r r

-- prop_responseDiff_in_range
--   :: TestResponse -> TestResponse -> Property
-- prop_responseDiff_in_range a b = property $ 0 <= x && x <= 1
--   where x = responseDiff a b

-- prop_makeResponseSimilar_works
--   :: TestResponse -> UIDouble -> TestResponse -> Property
-- prop_makeResponseSimilar_works
--   = prop_makeSimilar_works responseDiff makeResponseSimilar


test :: Test
test = testGroup "ALife.Creatur.Wain.ResponseQC"
  [
    testProperty "prop_serialize_round_trippable - TestResponse"
      (prop_serialize_round_trippable :: TestResponse -> Property),
    testProperty "prop_genetic_round_trippable - TestResponse"
      (prop_genetic_round_trippable (==)
       :: TestResponse -> Property),
    -- testProperty "prop_genetic_round_trippable2 - TestResponse"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestResponse -> Property),
    testProperty "prop_diploid_identity - TestResponse"
      (prop_diploid_identity (==) :: TestResponse -> Property),
    testProperty "prop_show_read_round_trippable - TestResponse"
      (prop_show_read_round_trippable (==) :: TestResponse -> Property),
    testProperty "prop_diploid_expressable - TestResponse"
      (prop_diploid_expressable
       :: TestResponse -> TestResponse -> Property),
    testProperty "prop_diploid_readable - TestResponse"
      (prop_diploid_readable :: TestResponse -> TestResponse -> Property),

    testProperty "prop_labelSimilarity_can_be_1"
      prop_labelSimilarity_can_be_1,
    testProperty "prop_labelSimilarity_can_be_0"
      prop_labelSimilarity_can_be_0
    -- -- testProperty "prop_responseDiff_can_be_1"
    -- --   prop_responseDiff_can_be_1,
    -- testProperty "prop_responseDiff_can_be_0"
    --   prop_responseDiff_can_be_0,
    -- testProperty "prop_responseDiff_in_range"
    --   prop_responseDiff_in_range,
    -- testProperty "prop_makeResponseSimilar_works"
    --   prop_makeResponseSimilar_works
  ]
