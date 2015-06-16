------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SOMQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.GeneticSOMQC
  (
    test,
    equivSOM,
    equivGSOM,
    sizedArbGeneticSOM,
    validExponential,
    equivExponential
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Wain.GeneticSOMInternal
import ALife.Creatur.Wain.TestUtils
import Control.Lens
import Control.Monad.Random (evalRand, runRand)
import Data.Datamining.Clustering.SSOM (SSOM, counter, toMap)
-- import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Serialize (Serialize)
import Data.Word (Word8, Word16)
import GHC.Generics (Generic)
import System.Random (mkStdGen)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen(MkGen))

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif

instance Arbitrary ExponentialParams where
  arbitrary = do
    p <- arbitrary
    MkGen (\r _ -> let (x,_) = runRand (randomExponential p) r in x)
    -- r0 <- choose r0RangeLimits
    -- d <- choose dRangeLimits
    -- return $ Exponential r0 d

equivExponential
  :: ExponentialParams -> ExponentialParams -> Bool
equivExponential a@(ExponentialParams r0a da)
                      b@(ExponentialParams r0b db)
  = abs (r0a - r0b) < (1/256)
    && abs (da - db) < (1/256)
    && validExponential a == validExponential b

data TestThinker = TestThinker Word8 deriving (Eq, Show, Generic)
  -- The parameter isn't used; it's just there to force something to
  -- be written to the gene sequence

instance Serialize TestThinker
instance W8.Genetic TestThinker
instance Diploid TestThinker

instance Arbitrary TestThinker where
  arbitrary = TestThinker <$> arbitrary

instance Thinker TestThinker where
  type Pattern TestThinker = TestPattern
  diff _ = testPatternDiff
  adjust _ = makeTestPatternSimilar

equivTestThinker :: TestThinker -> TestThinker -> Bool
equivTestThinker _ _ = True

setCounts
  :: [Word16] -> GeneticSOM p t -> GeneticSOM p t
setCounts ks (GeneticSOM s kMap e t) = GeneticSOM s' kMap' e t
  where kMap' = M.fromList . zip (M.keys kMap) $ ks
        s' = s { counter=(sum ks) }

type TestGSOM = GeneticSOM TestPattern TestThinker

sizedArbGeneticSOM
  :: Int -> Gen TestGSOM
sizedArbGeneticSOM n = do
  let s = n + 1
  f <- arbitrary
  xs <- vectorOf s (resize n arbitrary)
  t <- arbitrary
  let d = buildGeneticSOM f t xs
  ns <- vectorOf s (choose (0,25))
  return $ setCounts ns d

instance Arbitrary TestGSOM where
  arbitrary = sized sizedArbGeneticSOM

equivSOM
  :: Eq k
    => (p -> p -> Bool) -> SSOM t x k p -> SSOM t x k p -> Bool
equivSOM equivP x y = (length xs == length ys) && (and $ zipWith f xs ys)
  where f (a, b) (c, d) = (a == c) && (b `equivP` d)
        xs = M.toList . toMap $ x
        ys = M.toList . toMap $ y

equivGSOM
  :: (Eq p)
    => (p -> p -> Bool) -> (t -> t -> Bool) -> GeneticSOM p t -> GeneticSOM p t
      -> Bool
equivGSOM equivP equivT x y =
  equivGSOMIgnoringCounterMap equivP equivT x y
    && (view counterMap x) == (view counterMap y)

equivTestGSOM :: TestGSOM -> TestGSOM -> Bool
equivTestGSOM = equivGSOM (==) equivTestThinker

equivGSOMIgnoringCounterMap
  :: (Eq p)
    => (p -> p -> Bool) -> (t -> t -> Bool) -> GeneticSOM p t -> GeneticSOM p t
      -> Bool
equivGSOMIgnoringCounterMap equivP equivT x y =
  equivSOM equivP (view patternMap x) (view patternMap y)
    && equivExponential (view exponentialParams x)
        (view exponentialParams y)
    && equivT (view teacher x) (view teacher y)

equivTestGSOMIgnoringCounterMap :: TestGSOM -> TestGSOM -> Bool
equivTestGSOMIgnoringCounterMap
  = equivGSOMIgnoringCounterMap (==) equivTestThinker

instance Arbitrary RandomExponentialParams where
  arbitrary = do
    r0start <- arbitrary
    r0stop <- arbitrary
    dstart <- arbitrary
    dstop <- arbitrary
    return $ RandomExponentialParams (r0start,r0stop) (dstart,dstop)

prop_decayingExponential_valid :: ExponentialParams -> Property
prop_decayingExponential_valid f = property $ validExponential f

prop_random_decayingExponential_valid
  :: Int -> RandomExponentialParams -> Property
prop_random_decayingExponential_valid seed params
  = property $ validExponential f
  where g = mkStdGen seed
        f = evalRand (randomExponential params) g

prop_random_learning_rate_always_in_range
  :: ExponentialParams -> Word16 -> Property
prop_random_learning_rate_always_in_range f t =
  t >= 0 ==> 0 <= r && r <= 1
  where r = (toExponential f) t

prop_express_decayingExponential_valid
  :: ExponentialParams -> ExponentialParams -> Property
prop_express_decayingExponential_valid a b
  = property . validExponential $ express a b

prop_random_express_decayingExponential_valid
  :: Int -> RandomExponentialParams -> RandomExponentialParams -> Property
prop_random_express_decayingExponential_valid seed p1 p2
  = property . validExponential $ express a b
  where g = mkStdGen seed
        (a, g') = runRand (randomExponential p1) g
        b = evalRand (randomExponential p2) g'

prop_diploid_decayingExponential_valid
  :: ExponentialParams -> ExponentialParams -> Property
prop_diploid_decayingExponential_valid a b = property . validExponential $ c
  where g1 = W8.write a
        g2 = W8.write b
        Right c = W8.runDiploidReader W8.getAndExpress (g1, g2)

prop_sum_counts_correct
  :: TestGSOM -> [TestPattern] -> Property
prop_sum_counts_correct som ps = property $
  (sum . M.elems . _counterMap $ som') == (counter . _patternMap $ som')
  where som' = foldr runSOM som ps

runSOM :: TestPattern -> TestGSOM -> TestGSOM
runSOM p s = s'
  where (_, _, _, s') = reportAndTrain s p

-- The novelty won't decrease if the model is already close to the
-- input pattern and the learning rate isn't high enough.

prop_novelty_btw_0_and_1 :: TestPattern -> TestGSOM -> Property
prop_novelty_btw_0_and_1 p s = property $ 0 <= x && x <= 1
    where (_, _, x, _) = reportAndTrain s p

prop_familiar_patterns_have_min_novelty :: Word16 -> TestGSOM -> Property
prop_familiar_patterns_have_min_novelty k s = property $ x == 0
    where (_, _, x, _) = reportAndTrain s p
          p = s `modelAt` k'
          k' = k `mod` (fromIntegral $ numModels s)

prop_new_patterns_have_max_novelty :: ExponentialParams -> Int -> Property
prop_new_patterns_have_max_novelty f n = property $ x == 1
    where (_, _, x, _) = reportAndTrain s p
          s = buildGeneticSOM f (TestThinker 0) ps0
          ps0 = replicate (abs n + 1) (TestPattern 0)
          p = TestPattern 255

prop_novelty_decreases :: TestPattern -> TestGSOM -> Property
prop_novelty_decreases p s = x1 > 0.004 ==> x2 <= x1
    where (_, _, x1, s') = reportAndTrain s p
          (_, _, x2, _) = reportAndTrain s' p

prop_novelty_never_increases :: TestPattern -> TestGSOM -> Property
prop_novelty_never_increases p s = property $ x2 <= x1
    where (_, _, x1, s') = reportAndTrain s p
          (_, _, x2, _) = reportAndTrain s' p

prop_novelty_never_increases2 :: TestPattern -> TestPattern -> TestGSOM -> Property
prop_novelty_never_increases2 p1 p2 s = property $ x2 <= x1
    where (_, _, x1, s2) = reportAndTrain s p1
          (_, _, _, s3) = reportAndTrain s2 p2
          (_, _, x2, _) = reportAndTrain s3 p1

-- prop_novelty_never_increases3 :: [TestPattern] -> TestGSOM -> Property
-- prop_novelty_never_increases3 (p:ps) s = (not . null $ ps) ==> x2 <= x1
--     where (_, _, x1, s2) = reportAndTrain s p
--           s3 = trainAll s2 ps
--           (_, _, x2, _) = reportAndTrain s3 p

-- trainAll :: TestGSOM -> [TestPattern] -> TestGSOM
-- trainAll s ps = foldl' train1 s ps

-- train1 :: TestGSOM -> TestPattern -> TestGSOM
-- train1 s p = s'
--   where (_, _, _, s') = reportAndTrain s p

-- -- | WARNING: This can fail when two nodes are close enough in
-- --   value so that after training they become identical.
-- prop_classification_is_consistent
--   :: TestGSOM -> TestPattern -> Property
-- prop_classification_is_consistent s p = property $ bmu == bmu'
--   where (bmu, _, _, _, s') = reportAndTrain s p
--         (bmu', _, _, _, _) = reportAndTrain s' p

test :: Test
test = testGroup "ALife.Creatur.Wain.GeneticSOMQC"
  [
    testProperty "prop_serialize_round_trippable - Exponential"
      (prop_serialize_round_trippable
        :: ExponentialParams -> Property),
    testProperty "prop_genetic_round_trippable - Exponential"
      (prop_genetic_round_trippable equivExponential
        :: ExponentialParams -> Property),
    testProperty "prop_diploid_identity - Exponential"
      (prop_diploid_identity (==)
        :: ExponentialParams -> Property),

    testProperty "prop_serialize_round_trippable - GeneticSOM"
      (prop_serialize_round_trippable
        :: TestGSOM -> Property),
    testProperty "prop_genetic_round_trippable - GeneticSOM"
      (prop_genetic_round_trippable equivTestGSOM
        :: TestGSOM -> Property),
    testProperty "prop_diploid_identity - GeneticSOM"
      (prop_diploid_identity equivTestGSOMIgnoringCounterMap
        :: TestGSOM -> Property),
    testProperty "prop_diploid_expressable - GeneticSOM"
      (prop_diploid_expressable
        :: TestGSOM -> TestGSOM -> Property),
    testProperty "prop_diploid_readable - GeneticSOM"
      (prop_diploid_readable
        :: TestGSOM -> TestGSOM -> Property),

    testProperty "prop_decayingExponential_valid"
      prop_decayingExponential_valid,
    testProperty "prop_random_decayingExponential_valid"
      prop_random_decayingExponential_valid,
    testProperty "prop_random_learning_rate_always_in_range"
      prop_random_learning_rate_always_in_range,
    testProperty "prop_express_decayingExponential_valid"
      prop_express_decayingExponential_valid,
    testProperty "prop_random_express_decayingExponential_valid"
      prop_random_express_decayingExponential_valid,
    testProperty "prop_diploid_decayingExponential_valid"
      prop_diploid_decayingExponential_valid,
    testProperty "prop_sum_counts_correct"
      prop_sum_counts_correct,
    testProperty "prop_novelty_btw_0_and_1"
      prop_novelty_btw_0_and_1,
    testProperty "prop_familiar_patterns_have_min_novelty"
      prop_familiar_patterns_have_min_novelty,
    testProperty "prop_new_patterns_have_max_novelty"
      prop_new_patterns_have_max_novelty,
    testProperty "prop_novelty_decreases"
      prop_novelty_decreases,
    testProperty "prop_novelty_never_increases"
      prop_novelty_never_increases,
    testProperty "prop_novelty_never_increases2"
      prop_novelty_never_increases2
    -- testProperty "prop_new_som_has_models"
    --   prop_new_som_has_models
    -- testProperty "prop_classification_is_consistent"
    --   prop_classification_is_consistent
  ]
