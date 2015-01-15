------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DeciderQC
-- Copyright   :  (c) Amy de Buitléir 2013-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.DeciderQC
  (
    test,
    equiv
  ) where

import ALife.Creatur.Wain.Decider
import ALife.Creatur.Wain.GeneticSOM (reportAndTrain)
import ALife.Creatur.Wain.GeneticSOMQC (equiv)
import ALife.Creatur.Wain.Response (Response(..), setOutcome)
import ALife.Creatur.Wain.ResponseQC (TestAction)
import ALife.Creatur.Wain.Scenario (Scenario)
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.UnitInterval (UIDouble(..))
import ALife.Creatur.Wain.UnitIntervalQC ()
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type TestDecider = Decider TestAction

prop_training_makes_predictions_more_accurate
  :: TestDecider -> Scenario -> TestAction -> Double -> Property
prop_training_makes_predictions_more_accurate d s a o =
  a `elem` (knownActions d) ==> errAfter <= errBefore
  where (r, _) = predict d s a
        (Just predictionBefore) = outcome r
        errBefore = abs (o - predictionBefore)
        (_, _, d') = reportAndTrain d (r `setOutcome` o)
        (Just predictionAfter) = outcome . fst $ predict d' s a
        errAfter = abs (o - predictionAfter)

prop_prediction_error_in_range
  :: TestDecider -> Scenario -> TestAction -> UIDouble -> Property
prop_prediction_error_in_range d s a (UIDouble o) =
  a `elem` (knownActions d) ==> -2 <= e && e <= 2
  where (r, _) = predict d s a
        (Just prediction) = outcome r
        e = abs (o - prediction)

prop_imprint_works
  :: Decider TestAction -> Scenario -> TestAction -> Property
prop_imprint_works d s a
  = a `elem` (knownActions d) ==> x' >= x
  where d' = imprint d s a
        (r, _) = predict d s a
        (r', _) = predict d' s a
        Just x = outcome r
        Just x' = outcome r'

test :: Test
test = testGroup "ALife.Creatur.Wain.DeciderQC"
  [
    testProperty "prop_serialize_round_trippable - Decider"
      (prop_serialize_round_trippable :: TestDecider -> Property),
    testProperty "prop_genetic_round_trippable - Decider"
      (prop_genetic_round_trippable equiv :: TestDecider -> Property),
    testProperty "prop_diploid_identity - Decider"
      (prop_diploid_identity equiv :: TestDecider -> Property),
    testProperty "prop_diploid_expressable - Decider"
      (prop_diploid_expressable :: TestDecider -> TestDecider -> Property),
    testProperty "prop_diploid_readable - Decider"
      (prop_diploid_readable :: TestDecider -> TestDecider -> Property),
    testProperty "prop_training_makes_predictions_more_accurate"
      prop_training_makes_predictions_more_accurate,
    testProperty "prop_prediction_error_in_range"
      prop_prediction_error_in_range,
    testProperty "prop_imprint_works"
      prop_imprint_works
  ]
