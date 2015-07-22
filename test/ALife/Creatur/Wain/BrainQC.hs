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
import qualified ALife.Creatur.Wain.ClassifierQC as C
import qualified ALife.Creatur.Wain.DeciderQC as D
import ALife.Creatur.Wain.GeneticSOMQC ()
import ALife.Creatur.Wain.Response (Response(..), _outcome)
import ALife.Creatur.Wain.ResponseQC (TestAction, TestResponse)
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary (Brain TestPattern C.TestTweaker TestAction) where
  arbitrary = do
    c <- arbitrary
    d <- arbitrary
    hs <- arbitrary
    return $ Brain c d hs
    
equivBrain
  :: Brain TestPattern C.TestTweaker TestAction
    -> Brain TestPattern C.TestTweaker TestAction -> Bool
equivBrain b1 b2 = _classifier b1 `C.equivClassifier` _classifier b2
  && _decider b1 `D.equivDecider` _decider b2

prop_reflect_makes_predictions_more_accurate
  :: Brain TestPattern C.TestTweaker TestAction -> TestResponse -> [UIDouble]
    -> Property
prop_reflect_makes_predictions_more_accurate b r cAfter =
  property $ errAfter <= errBefore
  where ((r2, _):_, _) = predict b [r]
        (b2, errBefore) = reflect b r2 cAfter
        ((r3, _):_, _) = predict b [r]
        (_, errAfter) = reflect b2 r3 cAfter

prop_reflect_error_in_range
  :: Brain TestPattern C.TestTweaker TestAction -> TestResponse -> [UIDouble]
    -> Property
prop_reflect_error_in_range b r cAfter
  = property $ -2 <= x && x <= 2
  where (_, x) = reflect b r cAfter

prop_imprint_works
  :: Brain TestPattern C.TestTweaker TestAction -> [TestPattern]
    -> TestAction -> [UIDouble] -> Property
prop_imprint_works b ps a c = property $
  _outcome rAfter >= _outcome rBefore
  where (_, s, _, _) = assessSituation b ps c
        r = Response s a 0
        ((rBefore, _):_, _) = predict b [r]
        b' = imprint b ps a c
        ((rAfter, _):_, _) = predict b' [r]

test :: Test
test = testGroup "ALife.Creatur.Wain.BrainQC"
  [
    testProperty "prop_serialize_round_trippable - Brain"
      (prop_serialize_round_trippable
        :: Brain TestPattern C.TestTweaker TestAction -> Property),
    testProperty "prop_genetic_round_trippable - Brain"
      (prop_genetic_round_trippable equivBrain
        :: Brain TestPattern C.TestTweaker TestAction -> Property),
    testProperty "prop_diploid_identity - Brain"
      (prop_diploid_identity equivBrain
        :: Brain TestPattern C.TestTweaker TestAction -> Property),
    testProperty "prop_diploid_expressable - Brain"
      (prop_diploid_expressable
        :: Brain TestPattern C.TestTweaker TestAction
          -> Brain TestPattern C.TestTweaker TestAction -> Property),
    testProperty "prop_diploid_readable - Brain"
      (prop_diploid_readable
        :: Brain TestPattern C.TestTweaker TestAction
          -> Brain TestPattern C.TestTweaker TestAction -> Property),
    testProperty "prop_reflect_makes_predictions_more_accurate"
      prop_reflect_makes_predictions_more_accurate,
    testProperty "prop_reflect_error_in_range"
      prop_reflect_error_in_range,
    testProperty "prop_imprint_works"
      prop_imprint_works
  ]
