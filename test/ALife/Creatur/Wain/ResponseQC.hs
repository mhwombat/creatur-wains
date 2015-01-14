------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ResponseQC
-- Copyright   :  (c) Amy de Buitléir 2013-2014
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
    TestAction(..)
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.ResponseInternal
import ALife.Creatur.Wain.ScenarioQC ()
import ALife.Creatur.Wain.ConditionQC ()
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.Util (intersection)
import Control.Applicative
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Random (Random, random, randomR)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

data TestAction = Smile | Frown
  deriving (Show, Eq, Generic, Enum, Bounded)

instance Serialize TestAction
instance Genetic TestAction
instance Diploid TestAction

instance Arbitrary TestAction where
  arbitrary = elements [minBound .. maxBound]

type TestResponse = Response TestAction

instance Arbitrary TestResponse where
  arbitrary = do
    s <- arbitrary
    a <- arbitrary
    o <- arb8BitDouble (-1,1)
    return $ Response s a (Just o)

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

prop_diffOutcome_inRange :: TestOutcome -> TestOutcome -> Property
prop_diffOutcome_inRange (TestOutcome a) (TestOutcome b)
  = property $ 0 <= x && x <= 1
  where x = diffOutcome a b

prop_similarityIgnoringOutcome_inRange
  :: Response TestAction -> Response TestAction -> Property
prop_similarityIgnoringOutcome_inRange a b = property $ 0 <= x && x <= 1
  where x = similarityIgnoringOutcome a b

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
    testProperty "prop_diffOutcome_inRange"
      prop_diffOutcome_inRange,
    testProperty "prop_similarityIgnoringOutcome_inRange"
      prop_similarityIgnoringOutcome_inRange
  ]
