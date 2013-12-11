------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.WainQC
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.WainQC
  (
    test
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (runDiploidReader, write)
import ALife.Creatur.Wain
import qualified ALife.Creatur.Wain.BrainQC as B
import ALife.Creatur.Wain.ResponseQC (TestAction)
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity, TestPattern)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary Sex where
  arbitrary = elements [Male, Female]

equiv :: Wain TestPattern TestAction -> Wain TestPattern TestAction -> Bool
equiv a1 a2 =
  appearance a1 == appearance a2
  && brain a1 `B.equiv` brain a2
  && sex a1 == sex a2

strawMan :: Gen (Wain TestPattern TestAction)
strawMan = do
    a <- arbitrary
    b <- arbitrary
    s <- arbitrary
    c <- arbitrary
    return $ Wain "" a b s c ([],[])
  
-- | Can't just generate an arbitrary genome and build an agent from
--   it, because random genomes tend to be invalid.
instance Arbitrary (Wain TestPattern TestAction) where
  arbitrary = do
    n <- arbitrary
    a1 <- strawMan
    a2 <- strawMan
    let g1 = write a1
    let g2 = write a2
    let r = runDiploidReader (buildWain False n) (g1, g2)
    case r of
      (Left s)   -> error . show $ s
      (Right r') -> return r'

test :: Test
test = testGroup "ALife.WainQC"
  [
    testProperty "prop_serialize_round_trippable - Wain"
      (prop_serialize_round_trippable :: Wain TestPattern TestAction -> Property),
    testProperty "prop_genetic_round_trippable - Wain"
      (prop_genetic_round_trippable (equiv) :: Wain TestPattern TestAction -> Property),
    testProperty "prop_diploid_identity - Wain"
      (prop_diploid_identity (equiv) :: Wain TestPattern TestAction -> Property)
  ]
