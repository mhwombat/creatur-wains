------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ResponseQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module ALife.Creatur.Wain.ResponseQC
  (
    test,
    TestResponse,
    TestAction(..),
    sizedArbTestResponse
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.ResponseInternal
import ALife.Creatur.Wain.ScenarioQC (sizedArbScenario)
import ALife.Creatur.Wain.ConditionQC ()
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.UnitIntervalQC ()
import ALife.Creatur.Wain.Util (intersection)
import ALife.Creatur.Wain.Weights (Weights)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Random (Random, random, randomR)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

data TestAction = Walk | Run | Jump | Skip | Crawl
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance Serialize TestAction
instance Genetic TestAction
instance Diploid TestAction

instance Arbitrary TestAction where
  arbitrary = elements [minBound .. maxBound]

type TestResponse = Response TestAction

-- instance Serialize TestResponse
-- instance Genetic TestResponse
-- instance Diploid TestResponse

sizedArbTestResponse :: Int -> Gen TestResponse
sizedArbTestResponse n = do
  s <- sizedArbScenario n
  a <- arbitrary
  o <- arb8BitDouble (-1,1)
  return $ Response s a (Just o)

instance Arbitrary TestResponse where
  arbitrary = sized sizedArbTestResponse

-- | This is used for testing random SOM generation
instance Random TestAction where
  randomR (a,b) g = (toEnum n, g')
    where (n, g') = randomR (fromEnum a, fromEnum b) g
  random = randomR (minBound,maxBound)

newtype TestOutcome = TestOutcome Double deriving (Show, Eq)

instance Random TestOutcome where
  randomR (TestOutcome a, TestOutcome b) g = (TestOutcome x, g')
    where (x, g') = randomR (a', b') g
          (a', b') = intersection (-1,1) (a,b)
  random g = (TestOutcome x, g')
    where (x, g') = randomR (-1,1) g

instance Arbitrary TestOutcome where
  arbitrary = TestOutcome <$> choose (-1,1)

-- prop_responseDiff_can_be_1 :: Weights -> Property
-- prop_responseDiff_can_be_1 w = not (null ws) ==> abs (x - 1) < 1e-8
--   where x = responseDiff w (Response [0] [0] (Condition 0 0 0))
--               (Response [1] [1] (Condition 1 1 1))
--         ws = toDoubles w

prop_responseDiff_can_be_0
  :: Weights -> Weights -> Weights -> TestResponse -> Property
prop_responseDiff_can_be_0 cw sw rw r = property $ abs (x - 0) < 1e-8
  where x = responseDiff cw sw rw r r

prop_responseDiff_in_range
  :: Weights -> Weights -> Weights -> TestResponse -> TestResponse -> Property
prop_responseDiff_in_range cw sw rw a b = property $ 0 <= x && x <= 1
  where x = responseDiff cw sw rw a b

prop_similarityIgnoringOutcome_inRange
  :: Weights -> Weights -> Weights -> TestResponse -> TestResponse -> Property
prop_similarityIgnoringOutcome_inRange cw sw rw a b
  = property $ 0 <= x && x <= 1
  where x = similarityIgnoringOutcome cw sw rw a b

prop_makeResponseSimilar_works
  :: Weights -> Weights -> Weights -> TestResponse -> UIDouble
    -> TestResponse -> Property
prop_makeResponseSimilar_works cw sw rw
  = prop_makeSimilar_works (responseDiff cw sw rw) makeResponseSimilar


test :: Test
test = testGroup "ALife.Creatur.Wain.ResponseQC"
  [
    testProperty "prop_serialize_round_trippable - Response"
      (prop_serialize_round_trippable :: TestResponse -> Property),
    testProperty "prop_genetic_round_trippable - Response"
      (prop_genetic_round_trippable (==) :: TestResponse -> Property),
    testProperty "prop_diploid_identity - Response"
      (prop_diploid_identity (==) :: TestResponse -> Property),
    testProperty "prop_diploid_expressable - Response"
      (prop_diploid_expressable
       :: TestResponse -> TestResponse -> Property),
    testProperty "prop_diploid_readable - Response"
      (prop_diploid_readable :: TestResponse -> TestResponse -> Property),
    -- testProperty "prop_responseDiff_can_be_1"
    --   prop_responseDiff_can_be_1,
    testProperty "prop_responseDiff_can_be_0"
      prop_responseDiff_can_be_0,
    testProperty "prop_responseDiff_in_range"
      prop_responseDiff_in_range,
    testProperty "prop_similarityIgnoringOutcome_inRange"
      prop_similarityIgnoringOutcome_inRange,
    testProperty "prop_makeResponseSimilar_works"
      prop_makeResponseSimilar_works
  ]
