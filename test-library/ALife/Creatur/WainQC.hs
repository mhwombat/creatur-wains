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
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.WainQC
  (
    test
  ) where

import           ALife.Creatur.Gene.Numeric.UnitInterval (narrow)
import qualified ALife.Creatur.Gene.Test                 as GT
import           ALife.Creatur.Genetics.BRGCWord8        (runDiploidReader,
                                                          write)
import qualified ALife.Creatur.Wain.Brain                as B
import qualified ALife.Creatur.Wain.BrainQC              as BQC
import qualified ALife.Creatur.Wain.Classifier           as Cl
import           ALife.Creatur.Wain.GeneticSOM           (Label)
import           ALife.Creatur.Wain.PatternQC            (TestPattern,
                                                          TestPatternAdjuster)
import           ALife.Creatur.Wain.ResponseInternal     (labels)
import           ALife.Creatur.Wain.ResponseQC           (TestAction,
                                                          TestResponse,
                                                          TestResponseAdjuster)
import           ALife.Creatur.Wain.SimpleMuser          (SimpleMuser)
import           ALife.Creatur.WainInternal
import           Control.DeepSeq                         (deepseq)
import qualified Data.Datamining.Clustering.SGM4Internal as SOM
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)
import           Test.QuickCheck.Counterexamples         (Arbitrary, Gen,
                                                          Property, arbitrary,
                                                          choose, sized,
                                                          vectorOf)

type TestWain = Wain TestPatternAdjuster TestResponseAdjuster
                   TestPattern TestAction (SimpleMuser TestAction)

equiv
  :: TestWain
    -> TestWain
      -> Bool
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
                <*> arbitrary     -- children borne during lifetime
                <*> arbitrary     -- children weanded during lifetime
                <*> pure ([],[])  -- genome
                <*> pure []       -- biography

-- | Can't just generate an arbitrary genome and build an agent from
--   it, because random genomes tend to be invalid.
arbWain :: Gen TestWain
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

sizedArbWain :: Int -> Gen TestWain
sizedArbWain n = do
  w <- arbWain
  if n > 1
    then do
      k <- choose (1, min 3 n)
      cs <- vectorOf k arbWain
      return $ w { litter=cs }
    else return w

instance Arbitrary TestWain where
  arbitrary = sized sizedArbWain

prop_adjustEnergy_balances_energy
  :: Double -> TestWain -> Bool
prop_adjustEnergy_balances_energy e w
  = energy w' == energy w + narrow used
  where (w', used) = adjustEnergy e w

data ChoosingTestData
  = ChoosingTestData TestWain [TestPattern]
  deriving (Eq, Show)

sizedArbChoosingTestData :: Int -> Gen ChoosingTestData
sizedArbChoosingTestData n = do
  (BQC.ChoosingTestData b ps _) <- BQC.sizedArbChoosingTestData n
  w <- arbitrary
  let w' = w {brain = b}
  return $ ChoosingTestData w' ps

instance Arbitrary ChoosingTestData where
  arbitrary = sized sizedArbChoosingTestData

prop_chooseAction_never_causes_error
  :: ChoosingTestData -> Bool
prop_chooseAction_never_causes_error (ChoosingTestData w ps)
  = deepseq x True
  where x = chooseAction ps w

data ReflectionTestData
  = ReflectionTestData [TestPattern] TestResponse TestWain TestWain
  deriving (Eq, Show)

sizedArbReflectionTestData :: Int -> Gen ReflectionTestData
sizedArbReflectionTestData n = do
  (BQC.ReflectionTestData b r cBefore cAfter)
    <- BQC.sizedArbReflectionTestData n
  let nObjects = length $ labels r
  ps <- vectorOf nObjects arbitrary
  w <- arbitrary :: Gen TestWain
  let wBefore = w { brain = b, energy = head cBefore,
                    passion = 1 - cBefore !! 1 }
  let wAfter = wBefore { energy = head cAfter,
                         passion = 1 - cAfter !! 1 }
  return $ ReflectionTestData ps r wBefore wAfter

instance Arbitrary ReflectionTestData where
  arbitrary = sized sizedArbReflectionTestData

prop_reflect_never_causes_error
  :: ReflectionTestData -> Bool
prop_reflect_never_causes_error (ReflectionTestData _ r wBefore wAfter)
  = deepseq x True
  where x = reflect r wBefore wAfter

data ImprintTestData
  = ImprintTestData TestWain [TestPattern] [Label] TestAction B.Condition
    deriving (Eq, Show)

sizedArbImprintTestData :: Int -> Gen ImprintTestData
sizedArbImprintTestData n = do
  let nConditions = 3
  (BQC.ImprintTestData b ps a _ ls) <- BQC.sizedArbImprintTestData n
  w <- arbitrary
  let w' = w {brain = b}
  c <- vectorOf nConditions arbitrary
  return $ ImprintTestData w' ps ls a c

instance Arbitrary ImprintTestData where
  arbitrary = sized sizedArbImprintTestData

prop_imprintResponse_never_causes_error
  :: ImprintTestData -> Bool
prop_imprintResponse_never_causes_error (ImprintTestData w _ ls a _)
  = deepseq x True
  where x = imprintResponse ls a w

prop_imprintResponse_twice_never_causes_error
  :: ImprintTestData -> Bool
prop_imprintResponse_twice_never_causes_error (ImprintTestData w _ ls a _)
  = deepseq x True
  where (_, w') = imprintResponse ls a w
        x = imprintResponse ls a w'

-- prop_prettyClassifierModels_never_causes_error
--   :: ChoosingTestData -> Bool
-- prop_prettyClassifierModels_never_causes_error (ChoosingTestData w ps)
--   = deepseq x True
--   where (_, _, w') = chooseAction ps w
--         x = prettyClassifierModels w'

-- prop_prettyPredictorModels_never_causes_error
--   :: ChoosingTestData -> Bool
-- prop_prettyPredictorModels_never_causes_error (ChoosingTestData w ps)
--   = deepseq x True
--   where (_, _, w') = chooseAction ps w
--         x = prettyPredictorModels w'

-- prop_prettyClassificationReport_never_causes_error
--   :: ChoosingTestData -> Bool
-- prop_prettyClassificationReport_never_causes_error
--   (ChoosingTestData w ps)
--   = deepseq x' True
--   where (x, _, w') = chooseAction ps w
--         x' = prettyClassificationReport w' x

-- prop_prettyScenarioReport_never_causes_error
--   :: ChoosingTestData -> Bool
-- prop_prettyScenarioReport_never_causes_error (ChoosingTestData w ps)
--   = deepseq x' True
--   where (x, _, w') = chooseAction ps w
--         x' = prettyScenarioReport w' x

-- prop_prettyPredictionReport_never_causes_error
--   :: ChoosingTestData -> Bool
-- prop_prettyPredictionReport_never_causes_error (ChoosingTestData w ps)
--   = deepseq x' True
--   where (x, _, w') = chooseAction ps w
--         x' = prettyPredictionReport w' x

-- prop_prettyActionReport_never_causes_error
--   :: ChoosingTestData -> Bool
-- prop_prettyActionReport_never_causes_error (ChoosingTestData w ps)
--   = deepseq x' True
--   where (x, _, w') = chooseAction ps w
--         x' = prettyActionReport w' x

-- prop_prettyReflectionReport_never_causes_error
--   :: ReflectionTestData -> Bool
-- prop_prettyReflectionReport_never_causes_error
--   (ReflectionTestData ps r wBefore wAfter)
--   = deepseq x' True
--   where (x, w') = reflect ps r wBefore wAfter
--         x' = prettyReflectionReport w' x

-- prop_prettyImprintReport_never_causes_error
--   :: ImprintTestData -> Bool
-- prop_prettyImprintReport_never_causes_error
--   (ImprintTestData w ps a _)
--   = deepseq x' True
--   where (x, w') = imprint ps a w
--         x' = prettyImprintReport w' x

test :: Test
test = testGroup "ALife.Creatur.WainQC"
  [
    testProperty "prop_serialize_round_trippable - Wain"
      (GT.prop_serialize_round_trippable :: TestWain -> Bool),
    testProperty "prop_genetic_round_trippable - Wain"
      (GT.prop_genetic_round_trippable equiv :: TestWain -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - Wain"
    --   (GT.prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestWain -> Bool),
    testProperty "prop_diploid_identity - Wain"
      (GT.prop_diploid_identity equiv :: TestWain -> Bool),
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
      prop_imprintResponse_twice_never_causes_error
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
