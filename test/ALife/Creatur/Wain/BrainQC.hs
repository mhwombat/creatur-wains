------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.BrainQC
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.BrainQC
  (
    test,
    equiv
  ) where

import ALife.Creatur.Wain.BrainInternal
import ALife.Creatur.Wain.Condition
import qualified ALife.Creatur.Wain.ClassifierQC as C
import qualified ALife.Creatur.Wain.DeciderQC as D
import ALife.Creatur.Wain.GeneticSOMQC ()
import ALife.Creatur.Wain.ResponseQC (TestAction)
import ALife.Creatur.Wain.TestUtils
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary (Brain TestPattern TestAction) where
  arbitrary = do
    c <- arbitrary
    d <- arbitrary
    return $ buildBrain c d
    
equiv
  :: Brain TestPattern TestAction -> Brain TestPattern TestAction -> Bool
equiv b1 b2 = classifier b1 `C.equiv` classifier b2
  && decider b1 `D.equiv` decider b2

prop_feedback_makes_predictions_more_accurate
  :: TestPattern -> TestPattern -> (Condition, Condition)
    -> Brain TestPattern TestAction ->  Property
prop_feedback_makes_predictions_more_accurate p1 p2 cPair b =
  property $ errAfter <= errBefore
  where (b2, errBefore) = reflect p1 p2 cPair a b
        (_, errAfter) = reflect p1 p2 cPair a b2
        a = head . knownActions $ b

test :: Test
test = testGroup "ALife.Creatur.Wain.BrainQC"
  [
    testProperty "prop_serialize_round_trippable - Brain"
      (prop_serialize_round_trippable :: Brain TestPattern TestAction -> Property),
    testProperty "prop_genetic_round_trippable - Brain"
      (prop_genetic_round_trippable equiv :: Brain TestPattern TestAction -> Property),
    testProperty "prop_diploid_identity - Brain"
      (prop_diploid_identity equiv :: Brain TestPattern TestAction -> Property),
    testProperty "prop_diploid_expressable - Brain"
      (prop_diploid_expressable :: Brain TestPattern TestAction -> Brain TestPattern TestAction -> Property),
    testProperty "prop_diploid_readable - Brain"
      (prop_diploid_readable :: Brain TestPattern TestAction -> Brain TestPattern TestAction -> Property),
    testProperty "prop_feedback_makes_predictions_more_accurate"
      prop_feedback_makes_predictions_more_accurate
  ]
