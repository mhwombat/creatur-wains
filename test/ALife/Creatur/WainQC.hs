------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.WainQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
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
import ALife.Creatur.WainInternal
-- import ALife.Creatur.Wain.Brain (brainOK)
import qualified ALife.Creatur.Wain.BrainQC as BQC
import ALife.Creatur.Wain.ClassifierQC (TestThinker)
import ALife.Creatur.Wain.ResponseQC (TestAction)
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity, TestPattern)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Lens
-- import Control.Monad.Random (evalRand)
-- import System.Random (mkStdGen)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

equiv
  :: Wain TestPattern TestThinker TestAction
    -> Wain TestPattern TestThinker TestAction -> Bool
equiv a1 a2 =
  _appearance a1 == _appearance a2
  && _brain a1 `BQC.equivBrain` _brain a2
  && _devotion a1 == _devotion a2
  && _ageOfMaturity a1 == _ageOfMaturity a2
  && _passionDelta a1 == _passionDelta a2
--  && genome a1 == genome a2
--  && size a1 == size a2

strawMan :: Gen (Wain TestPattern TestThinker TestAction)
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
arbWain :: Gen (Wain TestPattern TestThinker TestAction)
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

sizedArbWain :: Int -> Gen (Wain TestPattern TestThinker TestAction)
sizedArbWain n = do
  w <- arbWain
  if n < 1
    then do
      cs <- listOf arbWain
      return $ set litter cs w
    else return w

instance Arbitrary (Wain TestPattern TestThinker TestAction) where
  arbitrary = sized sizedArbWain

-- No longer need this test because "mate" checks for abnormal brains
-- prop_offspring_are_valid
--   :: Wain TestPattern TestThinker TestAction -> Wain TestPattern TestThinker TestAction -> Int
--      -> Property
-- prop_offspring_are_valid a b seed = property . brainOK . brain $ c
--   where g = mkStdGen seed
--         Right c = evalRand (makeOffspring a b "fred") g

test :: Test
test = testGroup "ALife.Creatur.WainQC"
  [
    testProperty "prop_serialize_round_trippable - Wain"
      (prop_serialize_round_trippable
        :: Wain TestPattern TestThinker TestAction -> Property),
    testProperty "prop_genetic_round_trippable - Wain"
      (prop_genetic_round_trippable (equiv)
        :: Wain TestPattern TestThinker TestAction -> Property),
    testProperty "prop_diploid_identity - Wain"
      (prop_diploid_identity (equiv)
        :: Wain TestPattern TestThinker TestAction -> Property)
    -- testProperty "prop_offspring_are_valid" prop_offspring_are_valid
  ]
