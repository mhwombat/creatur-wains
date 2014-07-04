------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SOMQC
-- Copyright   :  (c) Amy de Buitléir 2013-2014
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
import Data.Datamining.Pattern (Pattern, Metric)
import Data.Datamining.Clustering.SOM (counter, DecayingGaussian(..))
import Data.Word (Word16)
import Math.Geometry.Grid (tileCount)
import Math.Geometry.Grid.Hexagonal (HexHexGrid, hexHexGrid)
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy (lazyGridMap)
import System.Random (mkStdGen)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary (DecayingGaussian Double) where
  arbitrary = do
    r0 <- choose unitInterval
    rf <- fmap (min r0) (choose unitInterval)
    w0 <- choose (1,255)
    wf <- fmap (min w0) (choose (0,255))
    tf <- arb8BitDouble (1,65535)
    return $ DecayingGaussian r0 rf w0 wf tf

equivDecayingGaussian
  :: DecayingGaussian Double -> DecayingGaussian Double -> Bool
equivDecayingGaussian (DecayingGaussian r0a rfa w0a wfa tfa)
                      (DecayingGaussian r0b rfb w0b wfb tfb)
  = abs (r0a - r0b) < (1/256)
    && abs (rfa - rfb) < (1/256)
    && abs (w0a - w0b) < 1
    && abs (wfa - wfb) < 1
    && abs (tfa - tfb) < 1

-- We want the number of tiles in a test grid to be O(n)
sizedHexHexGrid :: Int -> Gen HexHexGrid
sizedHexHexGrid n = do
  let s = isqrt (n `div` 3)
  return $ hexHexGrid s

instance Arbitrary HexHexGrid where
  arbitrary = sized sizedHexHexGrid

setCounts
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => [Word16] -> GeneticSOM p -> GeneticSOM p
setCounts ks (GeneticSOM s kMap) = GeneticSOM s' kMap'
  where kMap' = lazyGridMap (GM.toGrid kMap) ks
        s' = s { counter=(sum ks) }

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
equiv gs1 gs2 =
  learningFunction gs1 `equivDecayingGaussian` learningFunction gs2
    && numModels gs1 == numModels gs2
    && models gs1 == models gs2

instance Arbitrary RandomDecayingGaussianParams where
  arbitrary = do
    r0start <- choose (0,1)
    r0stop <- choose (0,1)
    rfstart <- choose (0,1)
    rfstop <- choose (0,1)
    w0start <- choose (0,1)
    w0stop <- choose (0,1)
    wfstart <- choose (0,1)
    wfstop <- choose (0,1)
    tfstart <- choose (0,100000000)
    tfstop <- choose (0,100000000)
    s <- arbitrary
    return $ RandomDecayingGaussianParams (r0start,r0stop)
      (rfstart,rfstop) (w0start,w0stop) (wfstart,wfstop)
      (tfstart,tfstop) s

prop_random_decayingGaussian_rf_le_r0
  :: Int -> RandomDecayingGaussianParams -> Property
prop_random_decayingGaussian_rf_le_r0 seed params = property $ rf <= r0
  where g = mkStdGen seed
        (DecayingGaussian r0 rf _ _ _)
           = evalRand (randomDecayingGaussian params) g
               :: DecayingGaussian Double

prop_random_decayingGaussian_wf_le_w0
  :: Int -> RandomDecayingGaussianParams -> Property
prop_random_decayingGaussian_wf_le_w0 seed params = property $ wf <= w0
  where g = mkStdGen seed
        (DecayingGaussian _ _ w0 wf _)
           = evalRand (randomDecayingGaussian params) g
               :: DecayingGaussian Double

prop_sum_counts_correct
  :: GeneticSOM TestPattern -> [TestPattern] -> Property
prop_sum_counts_correct som ps = property $
  (sum . GM.elems . counterMap $ som') == (counter . patternMap $ som')
  where som' = foldr runSOM som ps

runSOM :: TestPattern -> GeneticSOM TestPattern -> GeneticSOM TestPattern
runSOM p s = x
  where (_, _, x) = reportAndTrain s p

test :: Test
test = testGroup "ALife.Creatur.Wain.GeneticSOMQC"
  [
    testProperty "prop_serialize_round_trippable - DecayingGaussian"
      (prop_serialize_round_trippable
        :: DecayingGaussian Double -> Property),
    testProperty "prop_genetic_round_trippable - DecayingGaussian"
      (prop_genetic_round_trippable equivDecayingGaussian
        :: DecayingGaussian Double -> Property),
    testProperty "prop_diploid_identity - DecayingGaussian"
      (prop_diploid_identity (==)
        :: DecayingGaussian Double -> Property),

    testProperty "prop_serialize_round_trippable - HexHexGrid"
      (prop_serialize_round_trippable :: HexHexGrid -> Property),
    testProperty "prop_genetic_round_trippable - HexHexGrid"
      (prop_genetic_round_trippable (==) :: HexHexGrid -> Property),
    testProperty "prop_diploid_identity - HexHexGrid"
      (prop_diploid_identity (==) :: HexHexGrid -> Property),

    testProperty "prop_serialize_round_trippable - GeneticSOM"
      (prop_serialize_round_trippable
        :: GeneticSOM TestPattern -> Property),
    testProperty "prop_genetic_round_trippable - GeneticSOM"
      (prop_genetic_round_trippable equiv
        :: GeneticSOM TestPattern -> Property),
    testProperty "prop_diploid_identity - GeneticSOM"
      (prop_diploid_identity equiv
        :: GeneticSOM TestPattern -> Property),
    testProperty "prop_diploid_expressable - GeneticSOM"
      (prop_diploid_expressable
        :: GeneticSOM TestPattern -> GeneticSOM TestPattern -> Property),
    testProperty "prop_diploid_readable - GeneticSOM"
      (prop_diploid_readable
        :: GeneticSOM TestPattern -> GeneticSOM TestPattern -> Property),

    testProperty "prop_random_decayingGaussian_rf_le_r0"
      prop_random_decayingGaussian_rf_le_r0,
    testProperty "prop_random_decayingGaussian_wf_le_w0"
      prop_random_decayingGaussian_wf_le_w0,
    testProperty "prop_sum_counts_correct"
      prop_sum_counts_correct
    -- testProperty "prop_new_som_has_models"
    --   prop_new_som_has_models
  ]
