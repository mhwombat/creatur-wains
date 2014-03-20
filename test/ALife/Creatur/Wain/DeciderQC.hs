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
import ALife.Creatur.Wain.GeneticSOMQC (equiv)
import ALife.Creatur.Wain.TestUtils
import Control.Monad.Random (evalRand)
import Data.Word (Word8)
import System.Random (mkStdGen)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type TestDecider = Decider TestAction

prop_can_generate_random_decider :: Int -> Word8 -> Word8 -> Property
prop_can_generate_random_decider seed a b = property $
  som `seq` True
  where g = mkStdGen seed
        som = evalRand (randomDecider numClassifierModels maxDeciderSize) g :: TestDecider
        numClassifierModels = min 5 $ max 1 a
        maxDeciderSize = min 5 $ max 1 b

test :: Test
test = testGroup "ALife.Creatur.Wain.DeciderQC"
  [
    testProperty "prop_serialize_round_trippable - Decider"
      (prop_serialize_round_trippable :: TestDecider -> Property),
    testProperty "prop_genetic_round_trippable - Decider"
      (prop_genetic_round_trippable equiv :: TestDecider -> Property),
    testProperty "prop_diploid_identity - Decider"
      (prop_diploid_identity equiv :: TestDecider -> Property),
    testProperty "prop_diploid_expressable - Decider"
      (prop_diploid_expressable :: TestDecider -> TestDecider -> Property),
    testProperty "prop_diploid_readable - Decider"
      (prop_diploid_readable :: TestDecider -> TestDecider -> Property),
    testProperty "prop_can_generate_random_decider"
      prop_can_generate_random_decider
  ]
