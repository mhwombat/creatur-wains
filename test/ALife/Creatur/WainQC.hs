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
import ALife.Creatur.Wain.ClassifierQC (TestTweaker)
import ALife.Creatur.Wain.ResponseQC (TestAction)
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity, TestPattern)
import ALife.Creatur.Wain.UnitInterval (doubleToUI)
import ALife.Creatur.Wain.UnitIntervalQC (equivUIDouble)
import Control.Lens
-- import Control.Monad.Random (evalRand)
-- import System.Random (mkStdGen)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

equiv
  :: Wain TestPattern TestTweaker TestAction
    -> Wain TestPattern TestTweaker TestAction -> Bool
equiv a1 a2 =
  _appearance a1 == _appearance a2
  && _brain a1 `BQC.equivBrain` _brain a2
  && _devotion a1 `equivUIDouble` _devotion a2
  && _ageOfMaturity a1 == _ageOfMaturity a2
  && _passionDelta a1 `equivUIDouble` _passionDelta a2
--  && genome a1 == genome a2
--  && size a1 == size a2

strawMan :: Gen (Wain TestPattern TestTweaker TestAction)
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
                <*> arbitrary     -- size

-- | Can't just generate an arbitrary genome and build an agent from
--   it, because random genomes tend to be invalid.
arbWain :: Gen (Wain TestPattern TestTweaker TestAction)
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

sizedArbWain :: Int -> Gen (Wain TestPattern TestTweaker TestAction)
sizedArbWain n = do
  w <- arbWain
  if n < 1
    then do
      cs <- listOf arbWain
      return $ set litter cs w
    else return w

instance Arbitrary (Wain TestPattern TestTweaker TestAction) where
  arbitrary = sized sizedArbWain

-- No longer need this test because "mate" checks for abnormal brains
-- prop_offspring_are_valid
--   :: Wain TestPattern TestTweaker TestAction -> Wain TestPattern TestTweaker TestAction -> Int
--      -> Property
-- prop_offspring_are_valid a b seed = property . brainOK . brain $ c
--   where g = mkStdGen seed
--         Right c = evalRand (makeOffspring a b "fred") g

prop_adjustEnergy1_balances
  :: Double -> Wain TestPattern TestTweaker TestAction -> Property
prop_adjustEnergy1_balances e w = property $ e == used + leftover
  where (_, used, leftover) = adjustEnergy1 e w

prop_adjustEnergy_balances_adult_energy
  :: Double -> Wain TestPattern TestTweaker TestAction -> Property
prop_adjustEnergy_balances_adult_energy e w
  = property $ _energy w' == _energy w + doubleToUI adultShare
  where (w', adultShare, _) = adjustEnergy e w

prop_adjustEnergy_balances_child_energy
  :: Double -> Wain TestPattern TestTweaker TestAction -> Property
prop_adjustEnergy_balances_child_energy e w
  = property $ childEnergy w' == childEnergy w + doubleToUI childShare
  where (w', _, childShare) = adjustEnergy e w

prop_applyMetabolismCost_balances_adult_energy
  :: Wain TestPattern TestTweaker TestAction -> Double -> Double
    -> Double -> Property
prop_applyMetabolismCost_balances_adult_energy
  w baseCost costPerByte childCostFactor
    = property $ _energy w' == _energy w + doubleToUI adultShare
  where (w', adultShare, _)
          = applyMetabolismCost baseCost costPerByte childCostFactor w

prop_applyMetabolismCost_balances_child_energy
  :: Wain TestPattern TestTweaker TestAction -> Double -> Double
    -> Double -> Property
prop_applyMetabolismCost_balances_child_energy
  w baseCost costPerByte childCostFactor
    = property $ childEnergy w' == childEnergy w + doubleToUI childShare
  where (w', _, childShare)
          = applyMetabolismCost baseCost costPerByte childCostFactor w

test :: Test
test = testGroup "ALife.Creatur.WainQC"
  [
    testProperty "prop_serialize_round_trippable - Wain"
      (prop_serialize_round_trippable
        :: Wain TestPattern TestTweaker TestAction -> Property),
    testProperty "prop_genetic_round_trippable - Wain"
      (prop_genetic_round_trippable equiv
        :: Wain TestPattern TestTweaker TestAction -> Property),
    testProperty "prop_diploid_identity - Wain"
      (prop_diploid_identity equiv
        :: Wain TestPattern TestTweaker TestAction -> Property),
    testProperty "prop_adjustEnergy1_balances"
      prop_adjustEnergy1_balances,
    testProperty "prop_adjustEnergy_balances_adult_energy"
      prop_adjustEnergy_balances_adult_energy,
    testProperty "prop_adjustEnergy_balances_child_energy"
      prop_adjustEnergy_balances_child_energy,
    testProperty "prop_applyMetabolismCost_balances_adult_energy"
      prop_applyMetabolismCost_balances_adult_energy,
    testProperty "prop_applyMetabolismCost_balances_child_energy"
      prop_applyMetabolismCost_balances_child_energy
    -- testProperty "prop_offspring_are_valid" prop_offspring_are_valid
  ]
