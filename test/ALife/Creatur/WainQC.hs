------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.WainQC
-- Copyright   :  (c) Amy de Buitléir 2013-2016
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
import ALife.Creatur.WainInternal
import qualified ALife.Creatur.Wain.BrainQC as BQC
import ALife.Creatur.Wain.ClassifierQC (TestTweaker)
import ALife.Creatur.Wain.ResponseQC (TestAction)
import ALife.Creatur.Wain.SimpleMuser (SimpleMuser)
import ALife.Creatur.Wain.SimpleResponseTweaker (ResponseTweaker(..))
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity, TestPattern)
import ALife.Creatur.Wain.UnitInterval (doubleToUI)
import ALife.Creatur.Wain.UnitIntervalQC (equivUIDouble)
import Control.Lens
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type TestWain = Wain TestPattern TestTweaker (ResponseTweaker TestAction) SimpleMuser TestAction

equiv
  :: TestWain
    -> TestWain
      -> Bool
equiv a1 a2 =
  _appearance a1 == _appearance a2
  && _brain a1 `BQC.equivBrain` _brain a2
  && _devotion a1 `equivUIDouble` _devotion a2
  && _ageOfMaturity a1 == _ageOfMaturity a2
  && _passionDelta a1 `equivUIDouble` _passionDelta a2
--  && genome a1 == genome a2

strawMan :: Gen (TestWain)
strawMan = Wain <$> pure ""       -- name
                <*> arbitrary     -- appearance
                <*> arbitrary     -- brain
                <*> arbitrary     -- devotion
                <*> arbitrary     -- age of maturity
                <*> arbitrary     -- delta passion
                <*> arbitrary     -- delta boredom
                <*> arbitrary     -- energy
                <*> arbitrary     -- passion
                <*> arbitrary     -- boredom
                <*> arbitrary     -- age
                <*> pure []       -- litter
                <*> arbitrary     -- children borne during lifetime
                <*> arbitrary     -- children weanded during lifetime
                <*> pure ([],[])  -- genome

-- | Can't just generate an arbitrary genome and build an agent from
--   it, because random genomes tend to be invalid.
arbWain :: Gen (TestWain)
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

sizedArbWain :: Int -> Gen (TestWain)
sizedArbWain n = do
  w <- arbWain
  if n > 1
    then do
      k <- choose (1, min 3 n)
      cs <- vectorOf k arbWain
      return $ set litter cs w
    else return w

instance Arbitrary (TestWain) where
  arbitrary = sized sizedArbWain

prop_adjustEnergy_balances_energy
  :: Double -> TestWain -> Property
prop_adjustEnergy_balances_energy e w
  = property $ _energy w' == _energy w + doubleToUI used
  where (w', used) = adjustEnergy e w

test :: Test
test = testGroup "ALife.Creatur.WainQC"
  [
    testProperty "prop_serialize_round_trippable - Wain"
      (prop_serialize_round_trippable
        :: TestWain -> Property),
    testProperty "prop_genetic_round_trippable - Wain"
      (prop_genetic_round_trippable equiv
        :: TestWain -> Property),
    testProperty "prop_diploid_identity - Wain"
      (prop_diploid_identity equiv
        :: TestWain -> Property),
    testProperty "prop_adjustEnergy_balances_energy"
      prop_adjustEnergy_balances_energy
  ]
