------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DeciderQC
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.DeciderQC
  (
    test,
    equiv
  ) where

import ALife.Creatur.Wain.Decider
import ALife.Creatur.Wain.ResponseQC (TestAction)
import ALife.Creatur.Wain.GeneticSOM (Params(..), GeneticSOM(..),
  buildGeneticSOM)
import ALife.Creatur.Wain.GeneticSOMQC (equiv, sizedArbParams)
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity, arb8BitInt)
import Control.Monad.Random (evalRand)
import Data.Datamining.Clustering.SOM (setCounter)
import Data.Word (Word8)
import System.Random (mkStdGen)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type TestDecider = Decider TestAction

sizedTestDecider :: Int -> Gen (TestDecider)
sizedTestDecider n = do
  j <- choose (0,n)
  params <- sizedArbParams j
  let k = n - j
  xs <- vectorOf (fromIntegral . pSize $ params) (resize k arbitrary)
  t <- arb8BitInt (0, fromIntegral . pTf $ params)
  let d = buildGeneticSOM params xs
  return $ d { sSOM=setCounter t (sSOM d) }

instance Arbitrary TestDecider where
  arbitrary = sized sizedTestDecider

prop_can_generate_random_decider :: Int -> Word8 -> Word8 -> Property
prop_can_generate_random_decider seed numClassifierModels maxDeciderSize = property $
  som `seq` True
  where g = mkStdGen seed
        som = evalRand (randomDecider numClassifierModels maxDeciderSize) g :: TestDecider

test :: Test
test = testGroup "ALife.Creatur.Wain.DeciderQC"
  [
    testProperty "prop_serialize_round_trippable - Decider"
      (prop_serialize_round_trippable :: TestDecider -> Property),
    testProperty "prop_genetic_round_trippable - Decider"
      (prop_genetic_round_trippable equiv :: TestDecider -> Property),
    testProperty "prop_diploid_identity - Decider"
      (prop_diploid_identity equiv :: TestDecider -> Property),
    testProperty "prop_can_generate_random_decider"
      prop_can_generate_random_decider
  ]
