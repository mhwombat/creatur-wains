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
module ALife.Creatur.Wain.ResponseQC
  (
    test,
    TestResponse,
    TestAction(..),
    arbTestResponse
  ) where

import           ALife.Creatur.Gene.Test
import           ALife.Creatur.Genetics.BRGCWord8     (Genetic)
import           ALife.Creatur.Genetics.Diploid       (Diploid)
import           ALife.Creatur.Wain.GeneticSOM        (Label)
import           ALife.Creatur.Wain.Pretty            (Pretty)
import           ALife.Creatur.Wain.ResponseInternal
import           ALife.Creatur.Wain.Statistics        (Statistical (..))
import           Control.DeepSeq                      (NFData)
import           Data.Serialize                       (Serialize)
import           GHC.Generics                         (Generic)
import           System.Random                        (Random, random, randomR)
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

data TestAction = Walk | Run | Jump | Skip | Crawl
  deriving ( Show, Pretty, Read, Eq, Ord, Generic, Enum, Bounded,
             Serialize, Genetic, Diploid, NFData )

instance Arbitrary TestAction where
  arbitrary = elements [minBound .. maxBound]

instance Statistical TestAction where
  stats _ = []

type TestResponse = Response TestAction

-- TODO: Rewrite using sizedArbResponse from ALife.Creatur.Wain.ResponseInternal
sizedArbTestResponse :: Int -> Gen TestResponse
sizedArbTestResponse n = do
  nObjects <- choose (0, n)
  let nConditions = n - nObjects
  arbTestResponse nObjects nConditions

-- TODO: Rewrite using arbResponse from ALife.Creatur.Wain.ResponseInternal
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

prop_labelSimilarity_can_be_1 :: [Label] -> Bool
prop_labelSimilarity_can_be_1 xs = abs (x - 1) < 1e-8
  where x = labelSimilarity xs xs

prop_labelSimilarity_can_be_0 :: [Label] -> Property
prop_labelSimilarity_can_be_0 xs
  = (not . null $ xs) ==> abs x < 1e-8
  where x = labelSimilarity xs ys
        ys = map (+255) xs


test :: Test
test = testGroup "ALife.Creatur.Wain.ResponseQC"
  [
    testProperty "prop_serialize_round_trippable - TestResponse"
      (prop_serialize_round_trippable :: TestResponse -> Bool),
    testProperty "prop_genetic_round_trippable - TestResponse"
      (prop_genetic_round_trippable (==)
       :: TestResponse -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - TestResponse"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestResponse -> Bool),
    testProperty "prop_diploid_identity - TestResponse"
      (prop_diploid_identity (==) :: TestResponse -> Bool),
    testProperty "prop_show_read_round_trippable - TestResponse"
      (prop_show_read_round_trippable (==) :: TestResponse -> Bool),
    testProperty "prop_diploid_expressable - TestResponse"
      (prop_diploid_expressable
       :: TestResponse -> TestResponse -> Bool),
    testProperty "prop_diploid_readable - TestResponse"
      (prop_diploid_readable :: TestResponse -> TestResponse -> Bool),

    testProperty "prop_labelSimilarity_can_be_1"
      prop_labelSimilarity_can_be_1,
    testProperty "prop_labelSimilarity_can_be_0"
      prop_labelSimilarity_can_be_0
  ]
