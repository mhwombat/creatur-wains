------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.WainQC
-- Copyright   :  (c) Amy de Buitléir 2013-2019
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

import           ALife.Creatur.Genetics.BRGCWord8
    (runDiploidReader, write)
import qualified ALife.Creatur.Wain.Brain                 as B
import qualified ALife.Creatur.Wain.BrainQC               as BQC
import           ALife.Creatur.Wain.ClassifierQC
    (TestTweaker)
import           ALife.Creatur.Wain.ResponseInternal
    (labels)
import           ALife.Creatur.Wain.ResponseQC
    (TestAction, TestResponse)
import           ALife.Creatur.Wain.SimpleMuser
    (SimpleMuser)
import           ALife.Creatur.Wain.SimpleResponseTweaker
    (ResponseTweaker (..))
import           ALife.Creatur.Wain.TestUtils
    ( TestPattern
    , prop_diploid_identity
    , prop_genetic_round_trippable
    , prop_serialize_round_trippable
    )
import           ALife.Creatur.Wain.UnitInterval
    (doubleToUI)
import           ALife.Creatur.WainInternal
import           Control.DeepSeq
    (deepseq)
import           Control.Lens
import           Test.Framework
    (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2
    (testProperty)
import           Test.QuickCheck
    (Arbitrary, Gen, Property, arbitrary, choose, property, sized, vectorOf)

type TestWain = Wain TestPattern TestTweaker
                  (ResponseTweaker TestAction) (SimpleMuser TestAction)
                  TestAction

equiv
  :: TestWain
    -> TestWain
      -> Bool
equiv a1 a2 =
  _appearance a1 == _appearance a2
  && _brain a1 `BQC.equivBrain` _brain a2
  && _devotion a1 == _devotion a2
  && _ageOfMaturity a1 == _ageOfMaturity a2
  && _passionDelta a1 == _passionDelta a2
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

data ChoosingTestData
  = ChoosingTestData TestWain [TestPattern]

instance Show ChoosingTestData where
  show (ChoosingTestData w ps)
    = "ChoosingTestData (" ++ show w ++ ") " ++ show ps

sizedArbChoosingTestData :: Int -> Gen ChoosingTestData
sizedArbChoosingTestData n = do
  (BQC.ChoosingTestData b ps _) <- BQC.sizedArbChoosingTestData n
  w <- arbitrary
  let w' = w {_brain = b}
  return $ ChoosingTestData w' ps

instance Arbitrary ChoosingTestData where
  arbitrary = sized sizedArbChoosingTestData

prop_chooseAction_never_causes_error
  :: ChoosingTestData -> Property
prop_chooseAction_never_causes_error (ChoosingTestData w ps)
  = property $ deepseq x True
  where x = chooseAction ps w

data ReflectionTestData
  = ReflectionTestData [TestPattern] TestResponse TestWain TestWain

instance Show ReflectionTestData where
  show (ReflectionTestData ps a wBefore wAfter)
    = "ReflectionTestData (" ++ show ps ++ ") (" ++ show a ++ ") "
      ++ ") (" ++ show wBefore ++ ") ("  ++ show wAfter ++ ")"

sizedArbReflectionTestData :: Int -> Gen ReflectionTestData
sizedArbReflectionTestData n = do
  (BQC.ReflectionTestData b r cBefore cAfter)
    <- BQC.sizedArbReflectionTestData n
  let nObjects = length $ view labels r
  ps <- vectorOf nObjects arbitrary
  w <- arbitrary :: Gen TestWain
  let wBefore = w { _brain = b, _energy = head cBefore,
                    _passion = 1 - cBefore !! 1,
                    _boredom = cBefore !! 2}
  let wAfter = wBefore { _energy = head cAfter,
                         _passion = 1 - cAfter !! 1,
                         _boredom = cAfter !! 2}
  return $ ReflectionTestData ps r wBefore wAfter

instance Arbitrary ReflectionTestData where
  arbitrary = sized sizedArbReflectionTestData

prop_reflect_never_causes_error
  :: ReflectionTestData -> Property
prop_reflect_never_causes_error (ReflectionTestData ps r wBefore wAfter)
  = property $ deepseq x True
  where x = reflect ps r wBefore wAfter

data ImprintTestData
  = ImprintTestData TestWain [TestPattern] TestAction B.Condition
    deriving (Eq, Show)

sizedArbImprintTestData :: Int -> Gen ImprintTestData
sizedArbImprintTestData n = do
  let nConditions = 4
  (BQC.ImprintTestData b ps a _) <- BQC.sizedArbImprintTestData n
  w <- arbitrary
  let w' = w {_brain = b}
  c <- vectorOf nConditions arbitrary
  return $ ImprintTestData w' ps a c

instance Arbitrary ImprintTestData where
  arbitrary = sized sizedArbImprintTestData

prop_imprint_never_causes_error
  :: ImprintTestData -> Property
prop_imprint_never_causes_error (ImprintTestData w ps a _)
  = property $ deepseq x True
  where x = imprint ps a w

-- prop_prettyClassifierModels_never_causes_error
--   :: ChoosingTestData -> Property
-- prop_prettyClassifierModels_never_causes_error (ChoosingTestData w ps)
--   = property $ deepseq x True
--   where (_, _, w') = chooseAction ps w
--         x = prettyClassifierModels w'

-- prop_prettyPredictorModels_never_causes_error
--   :: ChoosingTestData -> Property
-- prop_prettyPredictorModels_never_causes_error (ChoosingTestData w ps)
--   = property $ deepseq x True
--   where (_, _, w') = chooseAction ps w
--         x = prettyPredictorModels w'

-- prop_prettyClassificationReport_never_causes_error
--   :: ChoosingTestData -> Property
-- prop_prettyClassificationReport_never_causes_error
--   (ChoosingTestData w ps)
--   = property $ deepseq x' True
--   where (x, _, w') = chooseAction ps w
--         x' = prettyClassificationReport w' x

-- prop_prettyScenarioReport_never_causes_error
--   :: ChoosingTestData -> Property
-- prop_prettyScenarioReport_never_causes_error (ChoosingTestData w ps)
--   = property $ deepseq x' True
--   where (x, _, w') = chooseAction ps w
--         x' = prettyScenarioReport w' x

-- prop_prettyPredictionReport_never_causes_error
--   :: ChoosingTestData -> Property
-- prop_prettyPredictionReport_never_causes_error (ChoosingTestData w ps)
--   = property $ deepseq x' True
--   where (x, _, w') = chooseAction ps w
--         x' = prettyPredictionReport w' x

-- prop_prettyActionReport_never_causes_error
--   :: ChoosingTestData -> Property
-- prop_prettyActionReport_never_causes_error (ChoosingTestData w ps)
--   = property $ deepseq x' True
--   where (x, _, w') = chooseAction ps w
--         x' = prettyActionReport w' x

-- prop_prettyReflectionReport_never_causes_error
--   :: ReflectionTestData -> Property
-- prop_prettyReflectionReport_never_causes_error
--   (ReflectionTestData ps r wBefore wAfter)
--   = property $ deepseq x' True
--   where (x, w') = reflect ps r wBefore wAfter
--         x' = prettyReflectionReport w' x

-- prop_prettyImprintReport_never_causes_error
--   :: ImprintTestData -> Property
-- prop_prettyImprintReport_never_causes_error
--   (ImprintTestData w ps a _)
--   = property $ deepseq x' True
--   where (x, w') = imprint ps a w
--         x' = prettyImprintReport w' x

test :: Test
test = testGroup "ALife.Creatur.WainQC"
  [
    testProperty "prop_serialize_round_trippable - Wain"
      (prop_serialize_round_trippable :: TestWain -> Property),
    testProperty "prop_genetic_round_trippable - Wain"
      (prop_genetic_round_trippable equiv :: TestWain -> Property),
    -- testProperty "prop_genetic_round_trippable2 - Wain"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestWain -> Property),
    testProperty "prop_diploid_identity - Wain"
      (prop_diploid_identity equiv :: TestWain -> Property),
    -- testProperty "prop_show_read_round_trippable - Wain"
    --   (prop_show_read_round_trippable (==) :: TestWain -> Property),

    testProperty "prop_adjustEnergy_balances_energy"
      prop_adjustEnergy_balances_energy,
    testProperty "prop_chooseAction_never_causes_error"
      prop_chooseAction_never_causes_error,
    testProperty "prop_reflect_never_causes_error"
      prop_reflect_never_causes_error,
    testProperty "prop_imprint_never_causes_error"
      prop_imprint_never_causes_error
    -- testProperty "prop_prettyClassifierModels_never_causes_error"
    --   prop_prettyClassifierModels_never_causes_error,
    -- testProperty "prop_prettyPredictorModels_never_causes_error"
    --   prop_prettyPredictorModels_never_causes_error,
    -- testProperty "prop_prettyClassificationReport_never_causes_error"
    --   prop_prettyClassificationReport_never_causes_error,
    -- testProperty "prop_prettyScenarioReport_never_causes_error"
    --   prop_prettyScenarioReport_never_causes_error,
    -- testProperty "prop_prettyPredictionReport_never_causes_error"
    --   prop_prettyPredictionReport_never_causes_error,
    -- testProperty "prop_prettyActionReport_never_causes_error"
    --   prop_prettyActionReport_never_causes_error,
    -- testProperty "prop_prettyReflectionReport_never_causes_error"
    --   prop_prettyReflectionReport_never_causes_error,
    -- testProperty "prop_prettyImprintReport_never_causes_error"
    --   prop_prettyImprintReport_never_causes_error
  ]
