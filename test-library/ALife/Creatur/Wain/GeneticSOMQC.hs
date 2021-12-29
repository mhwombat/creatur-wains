------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SOMQC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
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
    test,
    TestGSOM,
    sizedArbGeneticSOM,
    sizedArbEmptyGeneticSOM
  ) where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import qualified ALife.Creatur.Gene.Test                 as GT
import           ALife.Creatur.Wain.GeneticSOM
import           ALife.Creatur.Wain.PatternQC            (TestPattern,
                                                          TestPatternAdjuster)
import           Control.DeepSeq                         (deepseq)
import qualified Data.Datamining.Clustering.SGM4Internal as SOM
import           Data.Map.Strict                         (keys, (!))
import           Data.Word                               (Word32)
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)
import           Test.QuickCheck.Counterexamples         hiding (classify)

-- | @'classify' s p@ identifies the model @s@ that most closely
--   matches the pattern @p@.
--   It makes no changes to the GSOM.
--   (I.e., no training occurs, no new models are added,
--   the counts are not updated.)
--   For this reason, it should only be used for testing.
classify
  :: (SOM.Adjuster t, SOM.PatternType t ~ p, SOM.TimeType t ~ Word32,
     SOM.MetricType t ~ UI.UIDouble)
  => GeneticSOM t p -> p -> ClassificationDetail p
classify gs p = detail
  where (bmu, novelty, rs) = SOM.classify gs p
        age = SOM.time gs
        detail = ClassificationDetail
                   {
                     cPattern = p,
                     cBmu = bmu,
                     cBmuModel = gs `SOM.modelAt` bmu,
                     cNovelty = novelty,
                     cAdjNovelty = adjNovelty novelty age,
                     cDetails = rs
                   }

type TestGSOM = GeneticSOM TestPatternAdjuster TestPattern

-- | Used by other test modules
sizedArbEmptyGeneticSOM
  :: (Arbitrary t, SOM.Adjuster t)
  => Int -> Gen (GeneticSOM t p)
sizedArbEmptyGeneticSOM maxSz = do
  a <- arbitrary
  return $ SOM.makeSGM a maxSz

-- | Used by other test modules
sizedArbGeneticSOM
  :: (Arbitrary t, SOM.Adjuster t, SOM.PatternType t ~ p,
     SOM.MetricType t ~ UI.UIDouble, SOM.TimeType t ~ Word32)
  => Gen p -> Int -> Gen (GeneticSOM t p)
sizedArbGeneticSOM arbPattern n = do
  som <- sizedArbEmptyGeneticSOM (n+1)
  k <- choose (0, n+1)
  xs <- vectorOf k arbPattern
  return $ SOM.trainBatch som xs

instance Arbitrary TestGSOM where
  arbitrary = sized (sizedArbGeneticSOM arbitrary)

prop_classify_never_causes_error_unless_empty
  :: TestGSOM -> TestPattern -> Property
prop_classify_never_causes_error_unless_empty s p
  = not (SOM.isEmpty s) ==> deepseq x True
  where x = classify s p

prop_train_never_causes_error :: TestGSOM -> TestPattern -> Bool
prop_train_never_causes_error s p = deepseq (trainAndClassify s p) True

prop_imprint_never_causes_error :: TestGSOM -> Label -> TestPattern -> Bool
prop_imprint_never_causes_error s l p = deepseq (imprint s l p) True

prop_novelty_btw_0_and_1 :: TestPattern -> TestGSOM -> Bool
prop_novelty_btw_0_and_1 p s = 0 <= novelty && novelty <= 1
    where (r, _) = trainAndClassify s p
          novelty = cNovelty r

prop_familiar_patterns_have_min_novelty :: Int -> TestGSOM -> Property
prop_familiar_patterns_have_min_novelty k s
  = (not . SOM.isEmpty) s ==> novelty == 0
    where k' = k `mod` fromIntegral (SOM.numModels s)
          l = (keys . SOM.modelMap $ s) !! k'
          p = SOM.modelMap s ! l
          novelty = cNovelty $ classify s p

-- prop_new_patterns_have_max_novelty :: LearningParams -> Bool
-- prop_new_patterns_have_max_novelty e = novelty == 1
--     where s = buildGeneticSOM e 10 (TestTweaker 0)
--           novelty = cNovelty $ classify s (TestPattern 0)

-- -- The constraint is needed because the novelty won't decrease if the
-- -- model is already close to the input pattern and the learning rate
-- -- isn't high enough.
-- prop_novelty_decreases :: TestPattern -> GeneticSOM -> Bool
-- prop_novelty_decreases p s
--   = novelty1 > 0.004 ==> novelty2 <= novelty1
--     where (r1, s') = trainAndClassify s p
--           novelty1 = cNovelty r1
--           novelty2 = cNovelty $ classify s' p

prop_novelty_never_increases :: TestPattern -> TestGSOM -> Bool
prop_novelty_never_increases p s = novelty2 <= novelty1
    where (r1, s') = trainAndClassify s p
          novelty1 = cNovelty r1
          novelty2 = cNovelty $ classify s' p

-- | WARNING: This can fail when two nodes are close enough in
--   value so that after training they become identical.
--   If it does fail, just run it again.
prop_classification_is_consistent
  :: TestGSOM -> TestPattern -> Bool
prop_classification_is_consistent s p = bmu == bmu'
  where (r, s') = trainAndClassify s p
        bmu = cBmu r
        bmu' = cBmu $ classify s' p

test :: Test
test = testGroup "ALife.Creatur.Wain.GeneticSOMQC"
  [
    testProperty "prop_serialize_round_trippable - GeneticSOM"
      (GT.prop_serialize_round_trippable :: TestGSOM -> Bool),
    testProperty "prop_genetic_round_trippable - GeneticSOM"
      (GT.prop_genetic_round_trippable (==) :: TestGSOM -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - GeneticSOM"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestGSOM -> Bool),
    testProperty "prop_diploid_identity - GeneticSOM"
      (GT.prop_diploid_identity (==) :: TestGSOM -> Bool),
    testProperty "prop_show_read_round_trippable - GeneticSOM"
      (GT.prop_show_read_round_trippable (==) :: TestGSOM -> Bool),
    testProperty "prop_diploid_expressable - GeneticSOM"
      (GT.prop_diploid_expressable
       :: TestGSOM -> TestGSOM -> Bool),
    testProperty "prop_diploid_readable - GeneticSOM"
      (GT.prop_diploid_readable
       :: TestGSOM -> TestGSOM -> Bool),

    testProperty "prop_classify_never_causes_error_unless_empty"
      prop_classify_never_causes_error_unless_empty,
    testProperty "prop_train_never_causes_error"
      prop_train_never_causes_error,
    testProperty "prop_imprint_never_causes_error"
      prop_imprint_never_causes_error,
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
