------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.WainQC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.WainQC
  (
    test
  ) where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval    as UI
import qualified ALife.Creatur.Gene.Test                    as GT
import qualified ALife.Creatur.Genetics.BRGCWord8           as W8
import qualified ALife.Creatur.Genetics.Reproduction.Sexual as RS
import qualified ALife.Creatur.Wain.BrainQC                 as BQC
import           ALife.Creatur.Wain.PatternQC               (TestPattern,
                                                             TestPatternAdjuster)
import           ALife.Creatur.Wain.ResponseQC              (TestAction,
                                                             TestResponseAdjuster)
import           ALife.Creatur.Wain.SimpleMuser             (SimpleMuser)
import           ALife.Creatur.WainInternal
import           Control.DeepSeq                            (deepseq)
import           Control.Monad.Random                       (evalRand, mkStdGen)
import           Data.List                                  (foldl')
import qualified Numeric.ApproxEq                           as N
import           Test.Framework                             (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2       (testProperty)
import           Test.QuickCheck.Counterexamples            (Arbitrary, Gen,
                                                             arbitrary, getSize,
                                                             oneof, vectorOf)

type TestWain = Wain TestPatternAdjuster TestResponseAdjuster
                   TestPattern TestAction (SimpleMuser TestAction)

newtype InexperiencedTestWain = ITW TestWain deriving (Eq, Read, Show)

newtype ExperiencedTestWain = ETW TestWain deriving (Eq, Read, Show)

equiv :: TestWain -> TestWain -> Bool
equiv a1 a2 =
  appearance a1 == appearance a2
  && brain a1 `BQC.equivBrain` brain a2
  && devotion a1 == devotion a2
  && ageOfMaturity a1 == ageOfMaturity a2
  && passionDelta a1 == passionDelta a2
--  && genome a1 == genome a2

strawMan :: Gen TestWain
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
                <*> pure ([],[])  -- genome
                <*> pure []       -- biography

-- | Can't just generate an arbitrary genome and build an agent from
--   it, because random genomes tend to be invalid.
instance Arbitrary InexperiencedTestWain where
  arbitrary = do
    n <- arbitrary
    a1 <- strawMan
    a2 <- strawMan
    let g1 = W8.write a1
    let g2 = W8.write a2
    let r = W8.runDiploidReader (buildWainFromGenome False n) (g1, g2)
    case r of
      (Left s)   -> error . show $ s
      (Right r') -> return $ ITW r'

instance Arbitrary ExperiencedTestWain where
  arbitrary = do
    ITW w <- arbitrary
    n <- fmap (min 10) getSize
    experiences <- vectorOf n arbitrary
    let w' = foldl' runExperience w experiences
    return $ ETW w'

-- Avoid long input vectors, they slow down testing
arbitraryStimulus :: Gen [TestPattern]
arbitraryStimulus = do
  n <- fmap (\k -> min 3 (k + 1)) getSize
  vectorOf n arbitrary

data Experience = IncAgeExperience
                | EnergyAdjustmentExperience Double String
                | AutoAdjustPassionExperience
                | MatingExperience InexperiencedTestWain String
                | ChoiceExperience [TestPattern]
                | ReflectionExperience
                | ImprintResponseExperience [TestPattern] TestAction
                | WeaningExperience
                | PruningExperience
                | DeathExperience String
  deriving (Eq, Read, Show)

instance Arbitrary Experience where
  arbitrary = oneof [ return IncAgeExperience,
                      EnergyAdjustmentExperience <$> arbitrary <*> arbitrary,
                      return AutoAdjustPassionExperience,
                      MatingExperience <$> arbitrary <*> arbitrary,
                      ChoiceExperience <$> arbitraryStimulus,
                      return ReflectionExperience,
                      ImprintResponseExperience <$> arbitraryStimulus <*> arbitrary,
                      return WeaningExperience,
                      return PruningExperience,
                      DeathExperience <$> arbitrary ]

runExperience :: TestWain -> Experience -> TestWain
runExperience w IncAgeExperience = fst $ incAge w
runExperience w (EnergyAdjustmentExperience delta reason) = fst $ adjustEnergy delta reason w
runExperience w AutoAdjustPassionExperience = fst $ autoAdjustPassion w
runExperience w (MatingExperience (ITW w2) childName) = testMating w w2 childName
runExperience w (ChoiceExperience ps) = testChoice ps w
runExperience w ReflectionExperience = fst $ reflect w
runExperience w (ImprintResponseExperience ps a) = fst $ imprintResponse ps a w
runExperience w WeaningExperience = head . fst $ weanMatureChildren w
runExperience w PruningExperience = head . fst $ pruneDeadChildren w
runExperience w (DeathExperience reason) = fst $ recordDeath reason w

testMating :: TestWain -> TestWain -> String -> TestWain
testMating w w2 childName = w'
  where (w':_, _) = evalRand (mate w w2 childName) g
        g = mkStdGen 12345

testChoice :: [TestPattern] -> TestWain -> TestWain
testChoice ps w = w'
  where (w', _, _) =chooseAction ps w

-- This just tests the generator
prop_runExperience_never_causes_error
  :: InexperiencedTestWain -> Experience -> Bool
prop_runExperience_never_causes_error (ITW w) e
  = deepseq (runExperience w e) True

prop_serialize_round_trippable :: ExperiencedTestWain -> Bool
prop_serialize_round_trippable (ETW w)
  = GT.prop_serialize_round_trippable w

prop_genetic_round_trippable :: InexperiencedTestWain -> Bool
prop_genetic_round_trippable (ITW w)
  = GT.prop_genetic_round_trippable (equiv) w

prop_diploid_identity :: InexperiencedTestWain -> Bool
prop_diploid_identity (ITW w)
  = GT.prop_diploid_identity (equiv) w

prop_adjustEnergy_balances_energy
  :: Double -> UI.Double -> Bool
prop_adjustEnergy_balances_energy delta e
  = N.within 10 (UI.wide e') (UI.wide e + used)
  && N.within 10 delta (used + leftover)
  where (e', used, leftover) = adjustEnergy' delta e

data ChoosingTestData
  = ChoosingTestData ExperiencedTestWain [TestPattern]
  deriving (Eq, Show)

instance Arbitrary ChoosingTestData where
  arbitrary = do
    (BQC.ChoosingTestData b ps _) <- arbitrary
    (ETW w) <- arbitrary
    let w' = w {brain = b}
    return $ ChoosingTestData (ETW w') ps

prop_chooseAction_never_causes_error
  :: ChoosingTestData -> Bool
prop_chooseAction_never_causes_error (ChoosingTestData (ETW w) ps)
  = deepseq x True
  where x = chooseAction ps w

prop_reflect_never_causes_error :: ExperiencedTestWain -> Bool
prop_reflect_never_causes_error (ETW wAfter) = deepseq x True
  where x = reflect wAfter

prop_imprintResponse_never_causes_error
  :: ExperiencedTestWain -> [TestPattern] -> TestAction -> Bool
prop_imprintResponse_never_causes_error (ETW w) ps a = deepseq x True
  where x = imprintResponse ps a w

prop_imprintResponse_twice_never_causes_error
  :: ExperiencedTestWain -> [TestPattern] -> TestAction -> Bool
prop_imprintResponse_twice_never_causes_error (ETW w) ps a = deepseq w' True
  where w' = fst . imprintResponse ps a $ dummy
        dummy = fst . imprintResponse ps a $ w

prop_cloning_produces_equivalent_wain :: ExperiencedTestWain -> Bool
prop_cloning_produces_equivalent_wain (ETW w) = wFresh == wClone
  where wClone = RS.clone w (name w)
        wFresh = w { energy = 0,
                     passion = 1,
                     brain = brain wClone,
                     age = 0,
                     litter = [],
                     biography = [] }

prop_replay_produces_equivalent_wain :: ExperiencedTestWain -> Bool
prop_replay_produces_equivalent_wain (ETW w) = w == wClone'
  where wClone = fst $ replayLife w
        wClone' = wClone { name=name w }

test :: Test
test = testGroup "ALife.Creatur.WainQC"
  [
    testProperty "prop_runExperience_never_causes_error"
      prop_runExperience_never_causes_error,
    testProperty "prop_serialize_round_trippable - Wain"
      prop_serialize_round_trippable,
    testProperty "prop_genetic_round_trippable - Wain"
      prop_genetic_round_trippable,
    -- testProperty "prop_genetic_round_trippable2 - Wain"
    --   (GT.prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestWain -> Bool),
    testProperty "prop_diploid_identity - Wain"
      prop_diploid_identity,
    -- testProperty "prop_show_read_round_trippable - Wain"
    --   (GT.prop_show_read_round_trippable (==) :: TestWain -> Bool),
    testProperty "prop_adjustEnergy_balances_energy"
      prop_adjustEnergy_balances_energy,
    testProperty "prop_chooseAction_never_causes_error"
      prop_chooseAction_never_causes_error,
    testProperty "prop_reflect_never_causes_error"
      prop_reflect_never_causes_error,
    testProperty "prop_imprintResponse_never_causes_error"
      prop_imprintResponse_never_causes_error,
    testProperty "prop_imprintResponse_twice_never_causes_error"
      prop_imprintResponse_twice_never_causes_error,
    testProperty "prop_cloning_produces_equivalent_wain"
      prop_cloning_produces_equivalent_wain,
    testProperty "prop_replay_produces_equivalent_wain"
      prop_replay_produces_equivalent_wain
  ]
