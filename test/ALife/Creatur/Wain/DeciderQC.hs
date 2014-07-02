------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DeciderQC
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.DeciderQC
  (
    test,
    equiv
  ) where

import ALife.Creatur.Wain.Decider
import ALife.Creatur.Wain.GeneticSOMQC (equiv)
import ALife.Creatur.Wain.Response (Response(..))
import ALife.Creatur.Wain.ResponseQC (TestAction)
import ALife.Creatur.Wain.Scenario (Scenario)
import ALife.Creatur.Wain.TestUtils
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type TestDecider = Decider TestAction

-- prop_can_generate_random_decider :: Int -> Word8 -> Word8 -> Property
-- prop_can_generate_random_decider seed a b = property $
--   som `seq` True
--   where g = mkStdGen seed
--         som = evalRand (randomDecider numClassifierModels maxDeciderSize) g :: TestDecider
--         numClassifierModels = min 5 $ max 1 a
--         maxDeciderSize = min 5 $ max 1 b

prop_feedback_makes_predictions_more_accurate
  :: TestDecider -> Scenario -> TestAction -> Double -> Property
prop_feedback_makes_predictions_more_accurate d s a o =
  a `elem` (possibleActions d) ==> errAfter <= errBefore
  where response = Response s a (Just o)
        blankResponse = Response s a Nothing
        (Just predictionBefore) = outcome . fst $ predict d blankResponse
        errBefore = abs (o - predictionBefore)
        d' = feedback d response
        (Just predictionAfter) = outcome . fst $ predict d' blankResponse
        errAfter = abs (o - predictionAfter)

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
    testProperty "prop_feedback_makes_predictions_more_accurate"
      prop_feedback_makes_predictions_more_accurate
    -- testProperty "prop_can_generate_random_decider"
    --   prop_can_generate_random_decider
  ]
