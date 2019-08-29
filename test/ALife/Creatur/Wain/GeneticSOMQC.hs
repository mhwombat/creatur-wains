------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SOMQC
-- Copyright   :  (c) Amy de Buitléir 2013-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.GeneticSOMQC
  (
    TestTweaker(..),
    test,
    sizedArbGeneticSOM,
    sizedArbEmptyGeneticSOM,
    equivGSOM
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8      as W8
import           ALife.Creatur.Genetics.Diploid        (Diploid, express)
import           ALife.Creatur.Wain.GeneticSOMInternal
import           ALife.Creatur.Wain.TestUtils
import           ALife.Creatur.Wain.UnitIntervalQC     ()
import           Control.DeepSeq                       (NFData, deepseq)
import           Control.Lens
import           Control.Monad.Random                  (evalRand, runRand)
import           Data.Datamining.Clustering.SGM4       (toMap, trainBatch)
import           Data.Map.Strict                       (keys, (!))
import           Data.Serialize                        (Serialize)
import           Data.Word                             (Word64, Word8)
import           GHC.Generics                          (Generic)
import           System.Random                         (mkStdGen)
import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2  (testProperty)
import           Test.QuickCheck                       hiding
    (classify, maxSize)
import           Test.QuickCheck.Gen                   (Gen (MkGen))

instance Arbitrary LearningParams where
  arbitrary = do
    p <- arbitrary
    MkGen (\r _ -> let (x,_) = runRand (randomLearningParams p) r in x)

validLearningParams :: LearningParams -> Bool
validLearningParams (LearningParams r0 rf tf)
  = 0 < r0 && r0 <= 1 && 0 < rf && rf <= r0 && 0 < tf

prop_random_learning_rate_always_in_range
  :: LearningParams -> Word64 -> Property
prop_random_learning_rate_always_in_range f t = t >= 0 ==> deepseq r True
  where r = toLearningFunction f t

prop_express_learningFunction_valid
  :: LearningParams -> LearningParams -> Property
prop_express_learningFunction_valid a b
  = property . validLearningParams $ express a b

prop_diploid_learningFunction_valid
  :: LearningParams -> LearningParams -> Property
prop_diploid_learningFunction_valid a b = property . validLearningParams $ c
  where g1 = W8.write a
        g2 = W8.write b
        Right c = W8.runDiploidReader W8.getAndExpress (g1, g2)

prop_learningFunction_valid :: LearningParams -> Property
prop_learningFunction_valid f = property $ validLearningParams f

prop_learningFunction_always_valid
  :: LearningParams -> Word64 -> Property
prop_learningFunction_always_valid f t = property $ r >= 0 && r <= 1
  where r = toLearningFunction f t

instance Arbitrary LearningParamRanges where
  arbitrary = do
    r0start <- arbitrary
    r0stop <- arbitrary
    rfstart <- arbitrary
    rfstop <- arbitrary
    tfstart <- arbitrary
    tfstop <- arbitrary
    return $ LearningParamRanges (r0start,r0stop) (rfstart,rfstop)
               (tfstart,tfstop)

prop_random_learningFunction_valid
  :: Int -> LearningParamRanges -> Property
prop_random_learningFunction_valid seed params
  = property $ validLearningParams f
  where g = mkStdGen seed
        f = evalRand (randomLearningParams params) g

prop_random_express_learningFunction_valid
  :: Int -> LearningParamRanges -> LearningParamRanges -> Property
prop_random_express_learningFunction_valid seed p1 p2
  = property . validLearningParams $ express a b
  where g = mkStdGen seed
        (a, g') = runRand (randomLearningParams p1) g
        b = evalRand (randomLearningParams p2) g'

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

-- | Used by other test modules
sizedArbEmptyGeneticSOM
  :: (Arbitrary t, Tweaker t, p ~ Pattern t)
    => Int -> Gen (GeneticSOM p t)
sizedArbEmptyGeneticSOM maxSz = do
  e <- arbitrary
  t <- arbitrary
  return $ buildGeneticSOM e (fromIntegral maxSz) t

-- | Used by other test modules
sizedArbGeneticSOM
  :: (Arbitrary t, Tweaker t, Arbitrary p, p ~ Pattern t)
    => Gen p -> Int -> Gen (GeneticSOM p t)
sizedArbGeneticSOM arbPattern n = do
  som <- sizedArbEmptyGeneticSOM (n+1)
  k <- choose (0, n+1)
  xs <- vectorOf k arbPattern
  let s = trainBatch (_patternMap som) xs
  return $ som { _patternMap = s }

type TestGSOM = GeneticSOM TestPattern TestTweaker

instance Arbitrary TestGSOM where
  arbitrary = sized (sizedArbGeneticSOM arbitrary)

-- ignores counters and next index
equivGSOM :: Eq t => GeneticSOM p t -> GeneticSOM p t -> Bool
equivGSOM x y =
  -- TODO when initial models are made genetic: models x == models y
  maxSize x == maxSize y
    && (view learningParams x) == (view learningParams y)
    && (view tweaker x) == (view tweaker y)

prop_classify_never_causes_error_unless_empty
  :: TestGSOM -> TestPattern -> Property
prop_classify_never_causes_error_unless_empty s p
  = not (isEmpty s) ==> property $ deepseq x True
  where x = classify s p

prop_train_never_causes_error :: TestGSOM -> TestPattern -> Property
prop_train_never_causes_error s p
  = property $ deepseq (trainAndClassify s p) True

prop_imprint_never_causes_error :: TestGSOM -> Label -> TestPattern -> Property
prop_imprint_never_causes_error s l p
  = property $ deepseq (imprint s l p) True

prop_novelty_btw_0_and_1 :: TestPattern -> TestGSOM -> Property
prop_novelty_btw_0_and_1 p s
  = property $ 0 <= novelty && novelty <= 1
    where (r, _) = trainAndClassify s p
          novelty = cNovelty r

prop_familiar_patterns_have_min_novelty :: Int -> TestGSOM -> Property
prop_familiar_patterns_have_min_novelty k s
  = (not . isEmpty) s ==> novelty == 0
    where k' = k `mod` (fromIntegral $ numModels s)
          l = (keys . toMap . _patternMap $ s) !! k'
          p = modelMap s ! l
          novelty = cNovelty $ classify s p

-- prop_new_patterns_have_max_novelty :: LearningParams -> Property
-- prop_new_patterns_have_max_novelty e = property $ novelty == 1
--     where s = buildGeneticSOM e 10 (TestTweaker 0)
--           novelty = cNovelty $ classify s (TestPattern 0)

-- -- The constraint is needed because the novelty won't decrease if the
-- -- model is already close to the input pattern and the learning rate
-- -- isn't high enough.
-- prop_novelty_decreases :: TestPattern -> TestGSOM -> Property
-- prop_novelty_decreases p s
--   = novelty1 > 0.004 ==> novelty2 <= novelty1
--     where (r1, s') = trainAndClassify s p
--           novelty1 = cNovelty r1
--           novelty2 = cNovelty $ classify s' p

prop_novelty_never_increases :: TestPattern -> TestGSOM -> Property
prop_novelty_never_increases p s
  = property $ novelty2 <= novelty1
    where (r1, s') = trainAndClassify s p
          novelty1 = cNovelty r1
          novelty2 = cNovelty $ classify s' p

-- | WARNING: This can fail when two nodes are close enough in
--   value so that after training they become identical.
--   If it does fail, just run it again.
prop_classification_is_consistent
  :: TestGSOM -> TestPattern -> Property
prop_classification_is_consistent s p = property $ bmu == bmu'
  where (r, s') = trainAndClassify s p
        bmu = cBmu r
        bmu' = cBmu $ classify s' p

test :: Test
test = testGroup "ALife.Creatur.Wain.GeneticSOMQC"
  [
    testProperty "prop_serialize_round_trippable - TestPattern"
      (prop_serialize_round_trippable :: TestPattern -> Property),
    testProperty "prop_genetic_round_trippable - TestPattern"
      (prop_genetic_round_trippable (==) :: TestPattern -> Property),
    -- testProperty "prop_genetic_round_trippable2 - TestPattern"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestPattern -> Property),
    testProperty "prop_diploid_identity - TestPattern"
      (prop_diploid_identity (==) :: TestPattern -> Property),
    testProperty "prop_show_read_round_trippable - TestPattern"
      (prop_show_read_round_trippable (==) :: TestPattern -> Property),
    testProperty "prop_diploid_expressable - TestPattern"
      (prop_diploid_expressable :: TestPattern -> TestPattern -> Property),
    testProperty "prop_diploid_readable - TestPattern"
      (prop_diploid_readable :: TestPattern -> TestPattern -> Property),

    testProperty "prop_serialize_round_trippable - LearningParams"
      (prop_serialize_round_trippable :: LearningParams -> Property),
    testProperty "prop_genetic_round_trippable - LearningParams"
      (prop_genetic_round_trippable (==) :: LearningParams -> Property),
    -- testProperty "prop_genetic_round_trippable2 - LearningParams"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> LearningParams -> Property),
    testProperty "prop_diploid_identity - LearningParams"
      (prop_diploid_identity (==) :: LearningParams -> Property),
    testProperty "prop_show_read_round_trippable - LearningParams"
      (prop_show_read_round_trippable (==) :: LearningParams -> Property),
    testProperty "prop_diploid_expressable - LearningParams"
      (prop_diploid_expressable
       :: LearningParams -> LearningParams -> Property),
    testProperty "prop_diploid_readable - LearningParams"
      (prop_diploid_readable
       :: LearningParams -> LearningParams -> Property),

    testProperty "prop_serialize_round_trippable - TestGSOM"
      (prop_serialize_round_trippable :: TestGSOM -> Property),
    testProperty "prop_genetic_round_trippable - TestGSOM"
      (prop_genetic_round_trippable equivGSOM :: TestGSOM -> Property),
    -- testProperty "prop_genetic_round_trippable2 - TestGSOM"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestGSOM -> Property),
    testProperty "prop_diploid_identity - TestGSOM"
      (prop_diploid_identity equivGSOM :: TestGSOM -> Property),
    -- testProperty "prop_show_read_round_trippable - TestGSOM"
    --   (prop_show_read_round_trippable (==) :: TestGSOM -> Property),
    testProperty "prop_diploid_expressable - TestGSOM"
      (prop_diploid_expressable :: TestGSOM -> TestGSOM -> Property),
    testProperty "prop_diploid_readable - TestGSOM"
      (prop_diploid_readable :: TestGSOM -> TestGSOM -> Property),

    testProperty "prop_random_learning_rate_always_in_range"
      prop_random_learning_rate_always_in_range,
    testProperty "prop_express_learningFunction_valid"
      prop_express_learningFunction_valid,
    testProperty "prop_diploid_learningFunction_valid"
      prop_diploid_learningFunction_valid,
    testProperty "prop_learningFunction_valid"
      prop_learningFunction_valid,
    testProperty "prop_learningFunction_always_valid"
      prop_learningFunction_always_valid,
    testProperty "prop_random_learningFunction_valid"
      prop_random_learningFunction_valid,
    testProperty "prop_random_express_learningFunction_valid"
      prop_random_express_learningFunction_valid,
    testProperty "prop_classify_never_causes_error_unless_empty"
      prop_classify_never_causes_error_unless_empty,
    testProperty "prop_train_never_causes_error"
      prop_train_never_causes_error,
    testProperty "prop_novelty_btw_0_and_1"
      prop_novelty_btw_0_and_1,
    testProperty "prop_familiar_patterns_have_min_novelty"
      prop_familiar_patterns_have_min_novelty,
    -- testProperty "prop_new_patterns_have_max_novelty"
    --   prop_new_patterns_have_max_novelty,
    -- testProperty "prop_novelty_decreases"
    --   prop_novelty_decreases,
    testProperty "prop_novelty_never_increases"
      prop_novelty_never_increases,
    testProperty "prop_classification_is_consistent"
      prop_classification_is_consistent
  ]
