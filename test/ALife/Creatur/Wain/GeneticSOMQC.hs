------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SOMQC
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.GeneticSOMQC
  (
    test,
    equiv,
    sizedArbGeneticSOM
  ) where

import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.Util (unitInterval)
import ALife.Creatur.Util (isqrt)
import Control.Monad.Random (evalRand)
import qualified Data.Datamining.Clustering.Classifier as C
import Data.Datamining.Pattern (Pattern, Metric)
import Data.Datamining.Clustering.SOM (counter, DecayingGaussian(..),
  learningFunction)
import Math.Geometry.Grid (tileCount)
import Math.Geometry.Grid.Hexagonal (HexHexGrid, hexHexGrid)
import System.Random (mkStdGen)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary (DecayingGaussian Double) where
  arbitrary = do
    r0 <- arb8BitDouble unitInterval
    rf <- fmap (min r0) (arb8BitDouble unitInterval)
    w0 <- arb8BitDouble (0,255) -- (0,s)
    wf <- fmap (min w0) (arb8BitDouble (0,255))
    tf <- arb8BitDouble (0,65535)
    return $ DecayingGaussian r0 rf w0 wf tf

-- We want the number of tiles in a test grid to be O(n)
sizedHexHexGrid :: Int -> Gen HexHexGrid
sizedHexHexGrid n = do
  let s = isqrt (n `div` 3)
  return $ hexHexGrid s

instance Arbitrary HexHexGrid where
  arbitrary = sized sizedHexHexGrid

sizedArbGeneticSOM
  :: (Arbitrary p, Pattern p, Metric p ~ Double)
    => Int -> Gen (GeneticSOM p)
sizedArbGeneticSOM n = do
  let s = n `div` 30 + 1
  f <- arbitrary
  let numPatterns = tileCount $ hexHexGrid s
  xs <- vectorOf numPatterns (resize n arbitrary)
  let d = buildGeneticSOM (fromIntegral s) f xs
  ns <- vectorOf numPatterns (choose (0,25))
  return $ setCounts ns d

instance (Arbitrary p, Pattern p, Metric p ~ Double)
  => Arbitrary (GeneticSOM p) where
  arbitrary = sized sizedArbGeneticSOM

equiv
  :: (Eq p, Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> GeneticSOM p -> Bool
equiv (GeneticSOM s1 _) (GeneticSOM s2 _) =
  learningFunction s1 == learningFunction s2
    && C.numModels s1 == C.numModels s2
    && C.models s1 == C.models s2

prop_can_generate_random_geneticSOM :: Int -> Property
prop_can_generate_random_geneticSOM seed = property $
  som `seq` True
  where g = mkStdGen seed
        xs = [1..]
        som = evalRand (randomGeneticSOM 5 xs) g :: GeneticSOM Double

prop_sum_counts_correct
  :: GeneticSOM TestPattern -> [TestPattern] -> Property
prop_sum_counts_correct som ps = property $
  (sum . counts $ som') == (counter . sSOM $ som')
  where som' = foldr runSOM som ps

prop_som_always_has_models :: GeneticSOM TestPattern -> Property
prop_som_always_has_models som = property $ (not . null . models) som

runSOM :: TestPattern -> GeneticSOM TestPattern -> GeneticSOM TestPattern
runSOM p s = x
  where (_, _, x) = reportAndTrain s p

test :: Test
test = testGroup "ALife.Creatur.Wain.GeneticSOMQC"
  [
    testProperty "prop_serialize_round_trippable - DecayingGaussian"
      (prop_serialize_round_trippable :: DecayingGaussian Double -> Property),
    testProperty "prop_genetic_round_trippable - DecayingGaussian"
      (prop_genetic_round_trippable (==) :: DecayingGaussian Double -> Property),
    testProperty "prop_diploid_identity - DecayingGaussian"
      (prop_diploid_identity (==) :: DecayingGaussian Double -> Property),

    testProperty "prop_serialize_round_trippable - HexHexGrid"
      (prop_serialize_round_trippable :: HexHexGrid -> Property),
    testProperty "prop_genetic_round_trippable - HexHexGrid"
      (prop_genetic_round_trippable (==) :: HexHexGrid -> Property),
    testProperty "prop_diploid_identity - HexHexGrid"
      (prop_diploid_identity (==) :: HexHexGrid -> Property),

    testProperty "prop_serialize_round_trippable - GeneticSOM"
      (prop_serialize_round_trippable :: GeneticSOM TestPattern -> Property),
    testProperty "prop_genetic_round_trippable - GeneticSOM"
      (prop_genetic_round_trippable equiv :: GeneticSOM TestPattern -> Property),
    testProperty "prop_diploid_identity - GeneticSOM"
      (prop_diploid_identity equiv :: GeneticSOM TestPattern -> Property),
    testProperty "prop_diploid_expressable - GeneticSOM"
      (prop_diploid_expressable :: GeneticSOM TestPattern -> GeneticSOM TestPattern -> Property),
    testProperty "prop_diploid_readable - GeneticSOM"
      (prop_diploid_readable :: GeneticSOM TestPattern -> GeneticSOM TestPattern -> Property),

    testProperty "prop_can_generate_random_geneticSOM"
      prop_can_generate_random_geneticSOM,
    testProperty "prop_sum_counts_correct"
      prop_sum_counts_correct,
    testProperty "prop_som_always_has_models"
      prop_som_always_has_models
  ]
