------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.BrainQC
-- Copyright   :  (c) Amy de Buitléir 2013-2014
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
import ALife.Creatur.Wain.Scenario (Scenario)
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

prop_reflect_makes_predictions_more_accurate
  :: Brain TestPattern TestAction -> Scenario -> Condition
    -> Property
prop_reflect_makes_predictions_more_accurate b s cAfter =
  property $ errAfter <= errBefore
  where a = head . knownActions $ b
        (r, k) = predict b s a
        (b2, errBefore) = reflect b r k cAfter
        (_, errAfter) = reflect b2 r k cAfter

prop_reflect_error_in_range
  :: Brain TestPattern TestAction -> Scenario -> Condition
    -> Property
prop_reflect_error_in_range b s cAfter
  = property $ -2 <= x && x <= 2
  where a = head . knownActions $ b
        (r, k) = predict b s a
        (_, x) = reflect b r k cAfter

test :: Test
test = testGroup "ALife.Creatur.Wain.BrainQC"
  [
    testProperty "prop_serialize_round_trippable - Brain"
      (prop_serialize_round_trippable
        :: Brain TestPattern TestAction -> Property),
    testProperty "prop_genetic_round_trippable - Brain"
      (prop_genetic_round_trippable equiv
        :: Brain TestPattern TestAction -> Property),
    testProperty "prop_diploid_identity - Brain"
      (prop_diploid_identity equiv
        :: Brain TestPattern TestAction -> Property),
    testProperty "prop_diploid_expressable - Brain"
      (prop_diploid_expressable
        :: Brain TestPattern TestAction -> Brain TestPattern TestAction
          -> Property),
    testProperty "prop_diploid_readable - Brain"
      (prop_diploid_readable
        :: Brain TestPattern TestAction -> Brain TestPattern TestAction
          -> Property),
    testProperty "prop_reflect_makes_predictions_more_accurate"
      prop_reflect_makes_predictions_more_accurate,
    testProperty "prop_reflect_error_in_range"
      prop_reflect_error_in_range
  ]
