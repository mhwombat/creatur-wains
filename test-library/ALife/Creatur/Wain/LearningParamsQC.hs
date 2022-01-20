------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.LearningParamsQC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.LearningParamsQC
  (
    test
  ) where

import qualified ALife.Creatur.Gene.Test                   as GT
import qualified ALife.Creatur.Genetics.BRGCWord8          as W8
import           ALife.Creatur.Genetics.Diploid            (express)
import           ALife.Creatur.Wain.LearningParamsInternal
import           Control.DeepSeq                           (deepseq)
import           Control.Monad.Random                      (evalRand, runRand)
import           Data.Word                                 (Word32)
import           System.Random                             (mkStdGen)
import           Test.Framework                            (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2      (testProperty)
import           Test.QuickCheck.Counterexamples           (Arbitrary, Property,
                                                            arbitrary, (==>))
import           Test.QuickCheck.Gen                       (Gen (MkGen))

validLearningParams :: LearningParams -> Bool
validLearningParams (LearningParams r0 rf tf)
  = 0 < r0 && r0 <= 1 && 0 < rf && rf <= r0 && 0 < tf

prop_random_learning_rate_always_in_range
  :: LearningParams -> Word32 -> Property
prop_random_learning_rate_always_in_range f t = t >= 0 ==> deepseq r True
  where r = toLearningFunction f t

prop_express_learningFunction_valid
  :: LearningParams -> LearningParams -> Bool
prop_express_learningFunction_valid a b
  = validLearningParams $ express a b

prop_diploid_learningFunction_valid
  :: LearningParams -> LearningParams -> Bool
prop_diploid_learningFunction_valid a b = validLearningParams c
  where g1 = W8.write a
        g2 = W8.write b
        Right c = W8.runDiploidReader W8.getAndExpress (g1, g2)

prop_learningFunction_valid :: LearningParams -> Bool
prop_learningFunction_valid = validLearningParams

prop_learningFunction_always_valid
  :: LearningParams -> Word32 -> Bool
prop_learningFunction_always_valid f t = r >= 0 && r <= 1
  where r = toLearningFunction f t

prop_random_learningFunction_valid
  :: Int -> LearningParamRanges -> Bool
prop_random_learningFunction_valid seed params = validLearningParams f
  where g = mkStdGen seed
        f = evalRand (randomLearningParams params) g

prop_random_express_learningFunction_valid
  :: Int -> LearningParamRanges -> LearningParamRanges -> Bool
prop_random_express_learningFunction_valid seed p1 p2
  = validLearningParams $ express a b
  where g = mkStdGen seed
        (a, g') = runRand (randomLearningParams p1) g
        b = evalRand (randomLearningParams p2) g'

test :: Test
test = testGroup "ALife.Creatur.Wain.LearningParamsQC"
  [
    testProperty "prop_serialize_round_trippable - LearningParams"
      (GT.prop_serialize_round_trippable :: LearningParams -> Bool),
    testProperty "prop_genetic_round_trippable - LearningParams"
      (GT.prop_genetic_round_trippable (==) :: LearningParams -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - LearningParams"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> LearningParams -> Bool),
    testProperty "prop_diploid_identity - LearningParams"
      (GT.prop_diploid_identity (==) :: LearningParams -> Bool),
    testProperty "prop_show_read_round_trippable - LearningParams"
      (GT.prop_show_read_round_trippable (==) :: LearningParams -> Bool),
    testProperty "prop_diploid_expressable - LearningParams"
      (GT.prop_diploid_expressable
       :: LearningParams -> LearningParams -> Bool),
    testProperty "prop_diploid_readable - LearningParams"
      (GT.prop_diploid_readable
       :: LearningParams -> LearningParams -> Bool),

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
      prop_random_express_learningFunction_valid
  ]
