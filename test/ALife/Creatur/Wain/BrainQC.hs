------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.BrainQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.BrainQC
  (
    test,
    equivBrain
  ) where

import ALife.Creatur.Wain.BrainInternal
import ALife.Creatur.Wain.Condition
import qualified ALife.Creatur.Wain.ClassifierQC as C
import qualified ALife.Creatur.Wain.DeciderQC as D
import ALife.Creatur.Wain.GeneticSOMQC ()
import ALife.Creatur.Wain.Response (_outcome)
import ALife.Creatur.Wain.ResponseQC (TestAction)
import ALife.Creatur.Wain.Scenario (Scenario)
import ALife.Creatur.Wain.TestUtils
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary (Brain TestPattern C.TestThinker TestAction) where
  arbitrary = do
    c <- arbitrary
    d <- arbitrary
    hs <- arbitrary
    return $ Brain c d hs
    
equivBrain
  :: Brain TestPattern C.TestThinker TestAction
    -> Brain TestPattern C.TestThinker TestAction -> Bool
equivBrain b1 b2 = _classifier b1 `C.equivClassifier` _classifier b2
  && _decider b1 `D.equivDecider` _decider b2

prop_reflect_makes_predictions_more_accurate
  :: Brain TestPattern C.TestThinker TestAction -> Scenario -> Condition
    -> Property
prop_reflect_makes_predictions_more_accurate b s cAfter =
  property $ errAfter <= errBefore
  where a = head . knownActions $ b
        (r, _) = predict b s a
        (b2, errBefore) = reflect b r cAfter
        (_, errAfter) = reflect b2 r cAfter

prop_reflect_error_in_range
  :: Brain TestPattern C.TestThinker TestAction -> Scenario -> Condition
    -> Property
prop_reflect_error_in_range b s cAfter
  = property $ -2 <= x && x <= 2
  where a = head . knownActions $ b
        (r, _) = predict b s a
        (_, x) = reflect b r cAfter

prop_imprint_works
  :: Brain TestPattern C.TestThinker TestAction -> [TestPattern]
    -> TestAction -> Condition -> Property
prop_imprint_works b ps a c
  = a `elem` (knownActions b) ==> x' >= x
  where b' = imprint b ps a c
        (s, _, _) = assessSituation b ps c
        (r, _) = predict b s a
        (r', _) = predict b' s a
        Just x = _outcome r
        Just x' = _outcome r'

test :: Test
test = testGroup "ALife.Creatur.Wain.BrainQC"
  [
    testProperty "prop_serialize_round_trippable - Brain"
      (prop_serialize_round_trippable
        :: Brain TestPattern C.TestThinker TestAction -> Property),
    testProperty "prop_genetic_round_trippable - Brain"
      (prop_genetic_round_trippable equivBrain
        :: Brain TestPattern C.TestThinker TestAction -> Property),
    testProperty "prop_diploid_identity - Brain"
      (prop_diploid_identity equivBrain
        :: Brain TestPattern C.TestThinker TestAction -> Property),
    testProperty "prop_diploid_expressable - Brain"
      (prop_diploid_expressable
        :: Brain TestPattern C.TestThinker TestAction
          -> Brain TestPattern C.TestThinker TestAction -> Property),
    testProperty "prop_diploid_readable - Brain"
      (prop_diploid_readable
        :: Brain TestPattern C.TestThinker TestAction
          -> Brain TestPattern C.TestThinker TestAction -> Property),
    testProperty "prop_reflect_makes_predictions_more_accurate"
      prop_reflect_makes_predictions_more_accurate,
    testProperty "prop_reflect_error_in_range"
      prop_reflect_error_in_range,
    testProperty "prop_imprint_works"
      prop_imprint_works
  ]
