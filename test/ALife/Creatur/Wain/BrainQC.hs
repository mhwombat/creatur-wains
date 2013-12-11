------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.BrainQC
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.BrainQC
  (
    test,
    equiv
  ) where

import ALife.Creatur.Wain.Brain
import qualified ALife.Creatur.Wain.ClassifierQC as C
import qualified ALife.Creatur.Wain.DeciderQC as D
import ALife.Creatur.Wain.GeneticSOMQC ()
import ALife.Creatur.Wain.ResponseQC (TestAction)
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity, TestPattern)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary (Brain TestPattern TestAction) where
  arbitrary = do
    c <- arbitrary
    d <- arbitrary
    return $ buildBrain c d
    
equiv :: Brain TestPattern TestAction -> Brain TestPattern TestAction -> Bool
equiv b1 b2 = bClassifier b1 `C.equiv` bClassifier b2
  && bDecider b1 `D.equiv` bDecider b2

test :: Test
test = testGroup "ALife.Creatur.Wain.BrainQC"
  [
    testProperty "prop_serialize_round_trippable - Brain"
      (prop_serialize_round_trippable :: Brain TestPattern TestAction -> Property),
    testProperty "prop_genetic_round_trippable - Brain"
      (prop_genetic_round_trippable equiv :: Brain TestPattern TestAction -> Property),
    testProperty "prop_diploid_identity - Brain"
      (prop_diploid_identity equiv :: Brain TestPattern TestAction -> Property)
  ]
