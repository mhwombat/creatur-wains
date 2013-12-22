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
import Control.Applicative ((<$>), (<*>), pure)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

equiv :: Wain TestPattern TestAction -> Wain TestPattern TestAction -> Bool
equiv a1 a2 =
  appearance a1 == appearance a2
  && brain a1 `B.equiv` brain a2

maybeGiveChild :: Wain TestPattern TestAction -> Gen (Wain TestPattern TestAction)
maybeGiveChild w = do
  addChild <- arbitrary
  if addChild
    then do
      c <- arbWain
      return $ w { child=Just c}
    else return w

strawMan :: Gen (Wain TestPattern TestAction)
strawMan = Wain <$> pure ""       -- name
                <*> arbitrary     -- appearance
                <*> arbitrary     -- brain
                <*> arbitrary     -- age of maturity
                <*> arbitrary     -- condition
                <*> arbitrary     -- age
                <*> arbitrary     -- total number of children
                <*> pure Nothing  -- child
                <*> pure ([],[])  -- genome

-- | Can't just generate an arbitrary genome and build an agent from
--   it, because random genomes tend to be invalid.
arbWain :: Gen (Wain TestPattern TestAction)
arbWain = do
  n <- arbitrary
  a1 <- strawMan
  a2 <- strawMan
  let g1 = write a1
  let g2 = write a2
  let r = runDiploidReader (buildWain False n) (g1, g2)
  case r of
    (Left s)   -> error . show $ s
    (Right r') -> return r'

sizedArbWain :: Int -> Gen (Wain TestPattern TestAction)
sizedArbWain n = do
  w <- arbWain
  if n < 1
    then maybeGiveChild w
    else return w

instance Arbitrary (Wain TestPattern TestAction) where
  arbitrary = sized sizedArbWain
    

test :: Test
test = testGroup "ALife.Creatur.WainQC"
  [
    testProperty "prop_serialize_round_trippable - Wain"
      (prop_serialize_round_trippable :: Wain TestPattern TestAction -> Property),
    testProperty "prop_genetic_round_trippable - Wain"
      (prop_genetic_round_trippable (equiv) :: Wain TestPattern TestAction -> Property),
    testProperty "prop_diploid_identity - Wain"
      (prop_diploid_identity (equiv) :: Wain TestPattern TestAction -> Property)
  ]
