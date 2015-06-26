------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DeciderQC
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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.DeciderQC
  (
    test,
    equivThinker,
    equivDecider
  ) where

import ALife.Creatur.Wain.Decider
import ALife.Creatur.Wain.GeneticSOM (reportAndTrain, buildGeneticSOM)
import ALife.Creatur.Wain.GeneticSOMQC (equivGSOM)
import ALife.Creatur.Wain.Response (Response(..), setOutcome)
import ALife.Creatur.Wain.ResponseQC (TestAction, sizedArbTestResponse,
  equivResponse)
import ALife.Creatur.Wain.Scenario (Scenario)
import ALife.Creatur.Wain.ScenarioQC ()
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, pm1ToDouble)
import ALife.Creatur.Wain.WeightsQC (equivWeights)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type TestThinker = DeciderThinker TestAction

instance Arbitrary TestThinker where
  arbitrary
    = DeciderThinker <$> arbitrary <*> arbitrary <*> arbitrary

equivThinker :: TestThinker -> TestThinker -> Bool
equivThinker (DeciderThinker as bs cs) (DeciderThinker xs ys zs) =
  equivWeights as xs && equivWeights bs ys && equivWeights cs zs

type TestDecider = Decider TestAction

sizedArbTestDecider :: Int -> Gen TestDecider
sizedArbTestDecider n = do
  e <- arbitrary
  xs <- vectorOf (n+1) (sizedArbTestResponse $ min 5 n) -- reduce test time
  t <- arbitrary
  return $ buildGeneticSOM e t xs

instance Arbitrary TestDecider where
  arbitrary = sized sizedArbTestDecider

equivDecider :: TestDecider -> TestDecider -> Bool
equivDecider = equivGSOM equivResponse equivThinker


prop_training_makes_predictions_more_accurate
  :: TestDecider -> Scenario -> TestAction -> PM1Double -> Property
prop_training_makes_predictions_more_accurate d s a o =
  a `elem` (knownActions d) ==> errAfter <= errBefore
  where (r, _) = predict d s a
        (Just predictionBefore) = _outcome r
        errBefore = abs (pm1ToDouble o - pm1ToDouble predictionBefore)
        (_, _, _, d') = reportAndTrain d (r `setOutcome` o)
        (Just predictionAfter) = _outcome . fst $ predict d' s a
        errAfter = abs (pm1ToDouble o - pm1ToDouble predictionAfter)

prop_prediction_error_in_range
  :: TestDecider -> Scenario -> TestAction -> PM1Double -> Property
prop_prediction_error_in_range d s a o =
  a `elem` (knownActions d) ==> -2 <= e && e <= 2
  where (r, _) = predict d s a
        (Just prediction) = _outcome r
        e = abs (pm1ToDouble o - pm1ToDouble prediction)

prop_imprint_works
  :: TestDecider -> Scenario -> TestAction -> Property
prop_imprint_works d s a
  = a `elem` (knownActions d) ==> x' >= x
  where d' = imprint d s a
        (r, _) = predict d s a
        (r', _) = predict d' s a
        Just x = _outcome r
        Just x' = _outcome r'

test :: Test
test = testGroup "ALife.Creatur.Wain.DeciderQC"
  [
    testProperty "prop_serialize_round_trippable - Thinker"
      (prop_serialize_round_trippable :: TestThinker -> Property),
    testProperty "prop_genetic_round_trippable - Thinker"
      (prop_genetic_round_trippable equivThinker :: TestThinker -> Property),
    testProperty "prop_diploid_identity - Thinker"
      (prop_diploid_identity (==) :: TestThinker -> Property),
    testProperty "prop_diploid_expressable - Thinker"
      (prop_diploid_expressable :: TestThinker -> TestThinker -> Property),
    testProperty "prop_diploid_readable - Thinker"
      (prop_diploid_readable :: TestThinker -> TestThinker -> Property),
    testProperty "prop_serialize_round_trippable - Decider"
      (prop_serialize_round_trippable :: TestDecider -> Property),
    testProperty "prop_genetic_round_trippable - Decider"
      (prop_genetic_round_trippable equivDecider :: TestDecider -> Property),
    testProperty "prop_diploid_identity - Decider"
      (prop_diploid_identity (==) :: TestDecider -> Property),
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
