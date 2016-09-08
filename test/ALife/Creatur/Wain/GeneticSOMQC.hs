------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SOMQC
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.GeneticSOMQC
  (
    TestTweaker(..),
    test,
    equivGSOM,
    sizedArbGeneticSOM,
    sizedArbEmptyGeneticSOM,
    equivLearningFunction
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Wain.GeneticSOMInternal
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.UnitIntervalQC (equivUIDouble)
import Control.Lens
import Control.DeepSeq (NFData, deepseq)
import Control.Monad.Random (evalRand, runRand)
import Data.Datamining.Clustering.SGM (toMap, trainBatch)
import Data.Map.Strict (keys, (!))
import Data.Serialize (Serialize)
import Data.Word (Word8, Word64)
import GHC.Generics (Generic)
import System.Random (mkStdGen)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding (maxSize, classify)
import Test.QuickCheck.Gen (Gen(MkGen))

instance Arbitrary RandomLearningParams where
  arbitrary = do
    r0start <- arbitrary
    r0stop <- arbitrary
    rfstart <- arbitrary
    rfstop <- arbitrary
    tfstart <- arbitrary
    tfstop <- arbitrary
    return $ RandomLearningParams (r0start,r0stop) (rfstart,rfstop)
               (tfstart,tfstop)

instance Arbitrary LearningParams where
  arbitrary = do
    p <- arbitrary
    MkGen (\r _ -> let (x,_) = runRand (randomLearningFunction p) r in x)

equivLearningFunction
  :: LearningParams -> LearningParams -> Bool
equivLearningFunction (LearningParams r0a rfa tfa)
                 (LearningParams r0b rfb tfb)
  = equivUIDouble r0a r0b
    && equivUIDouble rfa rfb
    && tfa == tfb

data TestTweaker = TestTweaker Word8
  deriving (Eq, Show, Generic, Serialize, W8.Genetic, Diploid, NFData)
  -- The parameter isn't used; it's just there to force something to
  -- be written to the gene sequence

instance Arbitrary TestTweaker where
  arbitrary = TestTweaker <$> arbitrary

instance Tweaker TestTweaker where
  type Pattern TestTweaker = TestPattern
  diff _ = testPatternDiff
  adjust _ = makeTestPatternSimilar

equivTestTweaker :: TestTweaker -> TestTweaker -> Bool
equivTestTweaker _ _ = True

type TestGSOM = GeneticSOM TestPattern TestTweaker

sizedArbEmptyGeneticSOM
  :: (Arbitrary t, Tweaker t, p ~ Pattern t)
    => Int -> Gen (GeneticSOM p t)
sizedArbEmptyGeneticSOM maxSz = do
  e <- arbitrary
  dt <- (\x -> x*x) <$> arbitrary
  t <- arbitrary
  return $ buildGeneticSOM e (fromIntegral maxSz) dt t

sizedArbGeneticSOM
  :: (Arbitrary t, Tweaker t, Arbitrary p, p ~ Pattern t)
    => Gen p -> Int -> Gen (GeneticSOM p t)
sizedArbGeneticSOM arbPattern n = do
  som <- sizedArbEmptyGeneticSOM (n+1)
  k <- choose (0, n+1)
  xs <- vectorOf k arbPattern
  let s = trainBatch (_patternMap som) xs
  return $ som { _patternMap = s }

instance Arbitrary TestGSOM where
  arbitrary = sized (sizedArbGeneticSOM arbitrary)

-- ignores counters and next index
equivGSOM :: (t -> t -> Bool) -> GeneticSOM p t -> GeneticSOM p t -> Bool
equivGSOM equivT x y =
  -- TODO when initial models are made genetic: models x == models y
  maxSize x == maxSize y
    && equivUIDouble (diffThreshold x) (diffThreshold y)
    && equivLearningFunction (view learningParams x)
        (view learningParams y)
    &&  equivT (view tweaker x) (view tweaker y)

-- ignores counters and next index
equivTestGSOM :: TestGSOM -> TestGSOM -> Bool
equivTestGSOM = equivGSOM equivTestTweaker

validLearningFunction :: LearningParams -> Bool
validLearningFunction (LearningParams r0 rf tf)
  = 0 < r0 && r0 <= 1 && 0 < rf && rf <= r0 && 0 < tf

prop_learningFunction_valid :: LearningParams -> Property
prop_learningFunction_valid f = property $ validLearningFunction f

prop_random_learningFunction_valid
  :: Int -> RandomLearningParams -> Property
prop_random_learningFunction_valid seed params
  = property $ validLearningFunction f
  where g = mkStdGen seed
        f = evalRand (randomLearningFunction params) g

prop_random_learning_rate_always_in_range
  :: LearningParams -> Word64 -> Property
prop_random_learning_rate_always_in_range f t = t >= 0 ==> deepseq r True
  where r = toLearningFunction f t

prop_express_learningFunction_valid
  :: LearningParams -> LearningParams -> Property
prop_express_learningFunction_valid a b
  = property . validLearningFunction $ express a b

prop_random_express_learningFunction_valid
  :: Int -> RandomLearningParams -> RandomLearningParams -> Property
prop_random_express_learningFunction_valid seed p1 p2
  = property . validLearningFunction $ express a b
  where g = mkStdGen seed
        (a, g') = runRand (randomLearningFunction p1) g
        b = evalRand (randomLearningFunction p2) g'

prop_diploid_learningFunction_valid
  :: LearningParams -> LearningParams -> Property
prop_diploid_learningFunction_valid a b = property . validLearningFunction $ c
  where g1 = W8.write a
        g2 = W8.write b
        Right c = W8.runDiploidReader W8.getAndExpress (g1, g2)

prop_learningFunction_always_valid
  :: LearningParams -> Word64 -> Property
prop_learningFunction_always_valid f t = property $ r >= 0 && r <= 1
  where r = toLearningFunction f t

prop_train_never_causes_error :: TestGSOM -> TestPattern -> Property
prop_train_never_causes_error som p
  = property $ deepseq (train som p) True

prop_novelty_btw_0_and_1 :: TestPattern -> TestGSOM -> Property
prop_novelty_btw_0_and_1 p s = property $ 0 <= bmuDiff && bmuDiff <= 1
    where (_, bmuDiff, _, _) = trainAndClassify s p

prop_familiar_patterns_have_min_novelty :: Int -> TestGSOM -> Property
prop_familiar_patterns_have_min_novelty k s
  = (not . isEmpty) s ==> bmuDiff == 0
    where k' = k `mod` (fromIntegral $ numModels s)
          l = (keys . toMap . _patternMap $ s) !! k'
          p = modelMap s ! l
          (_, bmuDiff, _) = classify s p

-- This is impossible to test because the SOM will create a
-- new model if the BMU difference (i.e., the novelty) exceeds a
-- threshold.
-- prop_new_patterns_have_max_novelty :: LearningParams -> Property
-- prop_new_patterns_have_max_novelty e = property $ bmuDiff == 1
--     where s = buildGeneticSOM e 10 0.1 (TestTweaker 0)
--           (_, bmuDiff, _, _) = classify s (TestPattern 0)

-- The constraint is needed because the novelty won't decrease if the
-- model is already close to the input pattern and the learning rate
-- isn't high enough.
prop_novelty_decreases :: TestPattern -> TestGSOM -> Property
prop_novelty_decreases p s = bmuDiff1 > 0.004 ==> bmuDiff2 <= bmuDiff1
    where (_, bmuDiff1, _, s') = trainAndClassify s p
          (_, bmuDiff2, _) = classify s' p

prop_novelty_never_increases :: TestPattern -> TestGSOM -> Property
prop_novelty_never_increases p s = property $ x2 <= x1
    where (_, _, x1, s') = trainAndClassify s p
          (_, _, x2) = classify s' p

-- | WARNING: This can fail when two nodes are close enough in
--   value so that after training they become identical.
--   If it does fail, just run it again.
prop_classification_is_consistent
  :: TestGSOM -> TestPattern -> Property
prop_classification_is_consistent s p = property $ bmu == bmu'
  where (bmu, _, _, s') = trainAndClassify s p
        (bmu', _, _) = classify s' p

test :: Test
test = testGroup "ALife.Creatur.Wain.GeneticSOMQC"
  [
    testProperty "prop_makeSimilar_works - TestPattern"
      (prop_makeSimilar_works testPatternDiff makeTestPatternSimilar),
    testProperty "prop_serialize_round_trippable - LearningFunction"
      (prop_serialize_round_trippable
        :: LearningParams -> Property),
    testProperty "prop_genetic_round_trippable - LearningFunction"
      (prop_genetic_round_trippable equivLearningFunction
        :: LearningParams -> Property),
    testProperty "prop_diploid_identity - LearningFunction"
      (prop_diploid_identity (==)
        :: LearningParams -> Property),

    testProperty "prop_serialize_round_trippable - GeneticSOM"
      (prop_serialize_round_trippable :: TestGSOM -> Property),
    testProperty "prop_genetic_round_trippable - GeneticSOM"
      (prop_genetic_round_trippable equivTestGSOM
        :: TestGSOM -> Property),
    testProperty "prop_diploid_identity - GeneticSOM"
      (prop_diploid_identity equivTestGSOM
        :: TestGSOM -> Property),
    testProperty "prop_diploid_expressable - GeneticSOM"
      (prop_diploid_expressable
        :: TestGSOM -> TestGSOM -> Property),
    testProperty "prop_diploid_readable - GeneticSOM"
      (prop_diploid_readable
        :: TestGSOM -> TestGSOM -> Property),

    testProperty "prop_learningFunction_valid"
      prop_learningFunction_valid,
    testProperty "prop_random_learningFunction_valid"
      prop_random_learningFunction_valid,
    testProperty "prop_random_learning_rate_always_in_range"
      prop_random_learning_rate_always_in_range,
    testProperty "prop_express_learningFunction_valid"
      prop_express_learningFunction_valid,
    testProperty "prop_random_express_learningFunction_valid"
      prop_random_express_learningFunction_valid,
    testProperty "prop_diploid_learningFunction_valid"
      prop_diploid_learningFunction_valid,
    testProperty "prop_learningFunction_always_valid"
      prop_learningFunction_always_valid,
    testProperty "prop_train_never_causes_error"
      prop_train_never_causes_error,
    testProperty "prop_novelty_btw_0_and_1"
      prop_novelty_btw_0_and_1,
    testProperty "prop_familiar_patterns_have_min_novelty"
      prop_familiar_patterns_have_min_novelty,
    testProperty "prop_novelty_decreases"
      prop_novelty_decreases,
    testProperty "prop_novelty_never_increases"
      prop_novelty_never_increases,
    testProperty "prop_classification_is_consistent"
      prop_classification_is_consistent
  ]
