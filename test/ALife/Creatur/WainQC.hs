------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.WainQC
-- Copyright   :  (c) Amy de Buitléir 2013-2014
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
-- import ALife.Creatur.Genetics.Reproduction.Sexual (makeOffspring)
import ALife.Creatur.Wain
-- import ALife.Creatur.Wain.Brain (brainOK)
import qualified ALife.Creatur.Wain.BrainQC as BQC
import ALife.Creatur.Wain.ResponseQC (TestAction)
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity, TestPattern)
import Control.Applicative ((<$>), (<*>), pure)
-- import Control.Monad.Random (evalRand)
-- import System.Random (mkStdGen)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

equiv :: Wain TestPattern TestAction -> Wain TestPattern TestAction -> Bool
equiv a1 a2 =
  appearance a1 == appearance a2
  && brain a1 `BQC.equiv` brain a2
  && devotion a1 == devotion a2
  && ageOfMaturity a1 == ageOfMaturity a2
  && passionDelta a1 == passionDelta a2
--  && genome a1 == genome a2
--  && size a1 == size a2

strawMan :: Gen (Wain TestPattern TestAction)
strawMan = Wain <$> pure ""       -- name
                <*> arbitrary     -- appearance
                <*> arbitrary     -- brain
                <*> arbitrary     -- devotion
                <*> arbitrary     -- age of maturity
                <*> arbitrary     -- delta passion
                <*> arbitrary     -- energy
                <*> arbitrary     -- passion
                <*> arbitrary     -- age
                <*> pure []       -- litter
                <*> arbitrary     -- children borne during lifetime
                <*> arbitrary     -- children weanded during lifetime
                <*> arbitrary     -- # of wins
                <*> pure ([],[])  -- genome
                <*> arbitrary     -- size

-- | Can't just generate an arbitrary genome and build an agent from
--   it, because random genomes tend to be invalid.
arbWain :: Gen (Wain TestPattern TestAction)
arbWain = do
  n <- arbitrary
  a1 <- strawMan
  a2 <- strawMan
  let g1 = write a1
  let g2 = write a2
  let r = runDiploidReader (buildWainFromGenome False n) (g1, g2)
  case r of
    (Left s)   -> error . show $ s
    (Right r') -> return r'

sizedArbWain :: Int -> Gen (Wain TestPattern TestAction)
sizedArbWain n = do
  w <- arbWain
  if n < 1
    then do
      cs <- listOf arbWain
      return $ w { litter=cs}
    else return w

instance Arbitrary (Wain TestPattern TestAction) where
  arbitrary = sized sizedArbWain

-- No longer need this test because "mate" checks for abnormal brains
-- prop_offspring_are_valid
--   :: Wain TestPattern TestAction -> Wain TestPattern TestAction -> Int
--      -> Property
-- prop_offspring_are_valid a b seed = property . brainOK . brain $ c
--   where g = mkStdGen seed
--         Right c = evalRand (makeOffspring a b "fred") g

test :: Test
test = testGroup "ALife.Creatur.WainQC"
  [
    testProperty "prop_serialize_round_trippable - Wain"
      (prop_serialize_round_trippable :: Wain TestPattern TestAction -> Property),
    testProperty "prop_genetic_round_trippable - Wain"
      (prop_genetic_round_trippable (equiv) :: Wain TestPattern TestAction -> Property),
    testProperty "prop_diploid_identity - Wain"
      (prop_diploid_identity (equiv) :: Wain TestPattern TestAction -> Property)
    -- testProperty "prop_offspring_are_valid" prop_offspring_are_valid
  ]
