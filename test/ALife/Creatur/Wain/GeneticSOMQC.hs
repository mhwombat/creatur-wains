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
    sizedArbParams,
    sizedArbGeneticSOM
  ) where

import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.TestUtils (TestPattern, arb8BitInt,
  prop_serialize_round_trippable, prop_genetic_round_trippable,
  prop_diploid_identity)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.UnitIntervalQC ()
import Control.Monad.Random (evalRand)
import Data.Datamining.Clustering.SOM (setCounter)
import Data.Datamining.Pattern (Pattern, Metric)
import Data.Word (Word16)
import System.Random (mkStdGen)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

sizedArbParams :: Int -> Gen Params
sizedArbParams n = do
  let s = min 10 (n `div` 2 + 1)
  r0 <- arbitrary -- choose (0,1)
  rf <- arbitrary -- choose (r0,1)
  w0 <- arbitrary
  wf <- arbitrary
  tf <- arbitrary :: Gen Word16
  return $ Params (fromIntegral s) r0 rf w0 wf (fromIntegral tf)

instance Arbitrary Params where
  arbitrary = sized sizedArbParams

sizedArbGeneticSOM :: Int -> Gen (GeneticSOM TestPattern)
sizedArbGeneticSOM n = do
  j <- choose (0,n)
  params <- sizedArbParams j
  let k = n - j
  xs <- vectorOf (fromIntegral . pSize $ params) (resize k arbitrary)
  t <- arb8BitInt (0, fromIntegral . pTf $ params)
  let d = buildGeneticSOM params xs
  return $ d { sSOM=setCounter t (sSOM d) }

instance Arbitrary (GeneticSOM TestPattern) where
  arbitrary = sized sizedArbGeneticSOM

equiv :: (Eq p, Pattern p, Ord (Metric p)) => GeneticSOM p -> GeneticSOM p -> Bool
equiv s1 s2 = sParams s1 == sParams s2
                && numModels s1 == numModels s2
                && models s1 == models s2
                && sCounters s1 == sCounters s2

prop_can_generate_random_geneticSOM :: Int -> Property
prop_can_generate_random_geneticSOM seed = property $
  som `seq` True
  where g = mkStdGen seed
        xs = [1..]
        som = evalRand (randomGeneticSOM 5 xs) g :: GeneticSOM UIDouble

test :: Test
test = testGroup "ALife.Creatur.Wain.GeneticSOMQC"
  [
    testProperty "prop_serialize_round_trippable - Params"
      (prop_serialize_round_trippable :: Params -> Property),
    testProperty "prop_genetic_round_trippable - Params"
      (prop_genetic_round_trippable (==) :: Params -> Property),
    testProperty "prop_diploid_identity - Params"
      (prop_diploid_identity (==) :: Params -> Property),
    testProperty "prop_serialize_round_trippable - GeneticSOM"
      (prop_serialize_round_trippable :: GeneticSOM TestPattern -> Property),
    testProperty "prop_genetic_round_trippable - GeneticSOM"
      (prop_genetic_round_trippable equiv :: GeneticSOM TestPattern -> Property),
    testProperty "prop_diploid_identity - GeneticSOM"
      (prop_diploid_identity equiv :: GeneticSOM TestPattern -> Property),
    testProperty "prop_can_generate_random_geneticSOM"
      prop_can_generate_random_geneticSOM
  ]
