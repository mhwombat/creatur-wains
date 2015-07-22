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
    equivTweaker,
    equivDecider
  ) where

import ALife.Creatur.Wain.Decider
import ALife.Creatur.Wain.GeneticSOM (train)
import ALife.Creatur.Wain.GeneticSOMQC (equivGSOM)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, pm1ToDouble)
import ALife.Creatur.Wain.Response (Response(..))
import ALife.Creatur.Wain.ResponseQC (TestAction, TestResponse)
import ALife.Creatur.Wain.Scenario (Scenario)
import ALife.Creatur.Wain.ScenarioQC ()
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.GeneticSOMQC (sizedArbGeneticSOM)
import ALife.Creatur.Wain.WeightsQC (equivWeights)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type TestTweaker = DeciderTweaker TestAction

instance Arbitrary TestTweaker where
  arbitrary
    = DeciderTweaker <$> arbitrary <*> arbitrary <*> arbitrary

equivTweaker :: TestTweaker -> TestTweaker -> Bool
equivTweaker (DeciderTweaker as bs cs) (DeciderTweaker xs ys zs) =
  equivWeights as xs && equivWeights bs ys && equivWeights cs zs

type TestDecider = Decider TestAction

instance Arbitrary TestDecider where
  arbitrary = sized sizedArbGeneticSOM

equivDecider :: TestDecider -> TestDecider -> Bool
equivDecider = equivGSOM equivTweaker

prop_training_makes_predictions_more_accurate
  :: TestDecider -> TestResponse -> PM1Double -> Property
prop_training_makes_predictions_more_accurate d r o =
  property $ errAfter <= errBefore
  where (r2, _, d2) = predict d r
        predictionBefore = _outcome r2
        errBefore = abs (pm1ToDouble o - pm1ToDouble predictionBefore)
        rActual = r { _outcome = o }
        d3 = train d2 rActual
        (r4, _, _) = predict d3 r
        predictionAfter = _outcome r4
        errAfter = abs (pm1ToDouble o - pm1ToDouble predictionAfter)

prop_imprint_works
  :: TestDecider -> Scenario -> TestAction -> Property
prop_imprint_works d s a = property $ _outcome r' >= _outcome r
  where d' = imprint d s a
        r0 = Response s a 0
        (r, _, _) = predict d r0
        (r', _, _) = predict d' r0

test :: Test
test = testGroup "ALife.Creatur.Wain.DeciderQC"
  [
    testProperty "prop_serialize_round_trippable - Tweaker"
      (prop_serialize_round_trippable :: TestTweaker -> Property),
    testProperty "prop_genetic_round_trippable - Tweaker"
      (prop_genetic_round_trippable equivTweaker :: TestTweaker -> Property),
    testProperty "prop_diploid_identity - Tweaker"
      (prop_diploid_identity (==) :: TestTweaker -> Property),
    testProperty "prop_diploid_expressable - Tweaker"
      (prop_diploid_expressable :: TestTweaker -> TestTweaker -> Property),
    testProperty "prop_diploid_readable - Tweaker"
      (prop_diploid_readable :: TestTweaker -> TestTweaker -> Property),
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
    testProperty "prop_imprint_works"
      prop_imprint_works
  ]
