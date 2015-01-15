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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.GeneticSOMQC
  (
    test,
    equiv,
    sizedArbGeneticSOM,
    validExponential
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (express)
import ALife.Creatur.Wain.GeneticSOMInternal
import ALife.Creatur.Wain.TestUtils
import Control.Monad.Random (evalRand, runRand)
import Data.Datamining.Pattern (Pattern, Metric)
import Data.Datamining.Clustering.SSOMInternal (counter,
  Exponential(..), rate)
import qualified Data.Map.Strict as M
import Data.Word (Word16)
import System.Random (mkStdGen)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen(MkGen))

instance Arbitrary (Exponential Double) where
  arbitrary = do
    p <- arbitrary
    MkGen (\r _ -> let (x,_) = runRand (randomExponential p) r in x)
    -- r0 <- choose r0RangeLimits
    -- d <- choose dRangeLimits
    -- return $ Exponential r0 d

equivExponential
  :: Exponential Double -> Exponential Double -> Bool
equivExponential a@(Exponential r0a da)
                      b@(Exponential r0b db)
  = abs (r0a - r0b) < (1/256)
    && abs (da - db) < (1/256)
    && validExponential a == validExponential b

setCounts
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => [Word16] -> GeneticSOM p -> GeneticSOM p
setCounts ks (GeneticSOM s kMap) = GeneticSOM s' kMap'
  where kMap' = M.fromList . zip (M.keys kMap) $ ks
        s' = s { counter=(sum ks) }

sizedArbGeneticSOM
  :: (Arbitrary p, Pattern p, Metric p ~ Double)
    => Int -> Gen (GeneticSOM p)
sizedArbGeneticSOM n = do
  let s = n + 1
  f <- arbitrary
  xs <- vectorOf s (resize n arbitrary)
  let d = buildGeneticSOM f xs
  ns <- vectorOf s (choose (0,25))
  return $ setCounts ns d

instance (Arbitrary p, Pattern p, Metric p ~ Double)
  => Arbitrary (GeneticSOM p) where
  arbitrary = sized sizedArbGeneticSOM

equiv
  :: (Eq p, Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> GeneticSOM p -> Bool
equiv gs1 gs2 =
  learningFunction gs1 `equivExponential` learningFunction gs2
    && numModels gs1 == numModels gs2
    && models gs1 == models gs2

instance Arbitrary RandomExponentialParams where
  arbitrary = do
    r0start <- arbitrary
    r0stop <- arbitrary
    dstart <- arbitrary
    dstop <- arbitrary
    return $ RandomExponentialParams (r0start,r0stop) (dstart,dstop)

prop_decayingExponential_valid :: Exponential Double -> Property
prop_decayingExponential_valid f = property $ validExponential f

prop_random_decayingExponential_valid
  :: Int -> RandomExponentialParams -> Property
prop_random_decayingExponential_valid seed params
  = property $ validExponential f
  where g = mkStdGen seed
        f = evalRand (randomExponential params) g

prop_random_learning_rate_always_in_range
  :: Exponential Double -> Double -> Property
prop_random_learning_rate_always_in_range f t =
  t >= 0 ==> 0 <= r && r <= 1
  where r = rate f t

prop_express_decayingExponential_valid
  :: Exponential Double -> Exponential Double -> Property
prop_express_decayingExponential_valid a b
  = property . validExponential $ express a b

prop_random_express_decayingExponential_valid
  :: Int -> RandomExponentialParams -> RandomExponentialParams -> Property
prop_random_express_decayingExponential_valid seed p1 p2
  = property . validExponential $ express a b
  where g = mkStdGen seed
        (a, g') = runRand (randomExponential p1) g
        b = evalRand (randomExponential p2) g'

prop_diploid_decayingExponential_valid
  :: Exponential Double -> Exponential Double -> Property
prop_diploid_decayingExponential_valid a b = property . validExponential $ c
  where g1 = W8.write a
        g2 = W8.write b
        Right c = W8.runDiploidReader W8.getAndExpress (g1, g2)

prop_sum_counts_correct
  :: GeneticSOM TestPattern -> [TestPattern] -> Property
prop_sum_counts_correct som ps = property $
  (sum . M.elems . counterMap $ som') == (counter . patternMap $ som')
  where som' = foldr runSOM som ps

runSOM :: TestPattern -> GeneticSOM TestPattern -> GeneticSOM TestPattern
runSOM p s = x
  where (_, _, x) = reportAndTrain s p

-- -- | WARNING: This can fail when two nodes are close enough in
-- --   value so that after training they become identical.
-- prop_classification_is_consistent
--   :: GeneticSOM TestPattern -> TestPattern -> Property
-- prop_classification_is_consistent s p = property $ bmu == bmu'
--   where (bmu, _, _, _, s') = reportAndTrain s p
--         (bmu', _, _, _, _) = reportAndTrain s' p

test :: Test
test = testGroup "ALife.Creatur.Wain.GeneticSOMQC"
  [
    testProperty "prop_serialize_round_trippable - Exponential"
      (prop_serialize_round_trippable
        :: Exponential Double -> Property),
    testProperty "prop_genetic_round_trippable - Exponential"
      (prop_genetic_round_trippable equivExponential
        :: Exponential Double -> Property),
    testProperty "prop_diploid_identity - Exponential"
      (prop_diploid_identity (==)
        :: Exponential Double -> Property),

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

    testProperty "prop_decayingExponential_valid"
      prop_decayingExponential_valid,
    testProperty "prop_random_decayingExponential_valid"
      prop_random_decayingExponential_valid,
    testProperty "prop_random_learning_rate_always_in_range"
      prop_random_learning_rate_always_in_range,
    testProperty "prop_express_decayingExponential_valid"
      prop_express_decayingExponential_valid,
    testProperty "prop_random_express_decayingExponential_valid"
      prop_random_express_decayingExponential_valid,
    testProperty "prop_diploid_decayingExponential_valid"
      prop_diploid_decayingExponential_valid,
    testProperty "prop_sum_counts_correct"
      prop_sum_counts_correct
    -- testProperty "prop_new_som_has_models"
    --   prop_new_som_has_models
    -- testProperty "prop_classification_is_consistent"
    --   prop_classification_is_consistent
  ]
