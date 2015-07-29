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
import qualified ALife.Creatur.Wain.PredictorQC as D
import ALife.Creatur.Wain.GeneticSOMQC (sizedArbGeneticSOM)
import ALife.Creatur.Wain.Muser (mostLikelyScenarios)
import ALife.Creatur.Wain.MuserQC ()
import ALife.Creatur.Wain.Response (Response(..), _outcome)
import ALife.Creatur.Wain.ResponseQC (TestAction, TestResponse)
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type TestBrain = Brain TestPattern C.TestTweaker TestAction

sizedArbTestBrain :: Int -> Gen TestBrain
sizedArbTestBrain n = do
  cSize <- choose (0, n)
  nObjects <- choose (0, n - cSize)
  nConditions <- choose (0, n - cSize - nObjects)
  let pSize = n - cSize - nObjects - nConditions
  arbTestBrain cSize nObjects nConditions pSize

arbTestBrain :: Int -> Int -> Int -> Int -> Gen TestBrain
arbTestBrain cSize nObjects nConditions pSize = do
  c <- sizedArbGeneticSOM arbitrary cSize
  pos <- arbitrary
  p <- D.arbTestPredictor nObjects nConditions pSize
  hw <- arbitrary
  return $ Brain c pos p hw

instance Arbitrary (TestBrain) where
  arbitrary = sized sizedArbTestBrain

equivBrain :: TestBrain -> TestBrain -> Bool
equivBrain b1 b2 = _classifier b1 `C.equivClassifier` _classifier b2
  && _predictor b1 `D.equivPredictor` _predictor b2

prop_reflect_makes_predictions_more_accurate
  :: TestBrain -> TestResponse -> [UIDouble] -> Property
prop_reflect_makes_predictions_more_accurate b r cAfter =
  property $ errAfter <= errBefore
  where ((r2, _):_, _) = predictAll b . zip [r] $ repeat 1
        (b2, errBefore) = reflect b r2 cAfter
        ((r3, _):_, _) = predictAll b . zip [r] $ repeat 1
        (_, errAfter) = reflect b2 r3 cAfter

prop_reflect_error_in_range
  :: TestBrain -> TestResponse -> [UIDouble] -> Property
prop_reflect_error_in_range b r cAfter
  = property $ -2 <= x && x <= 2
  where (_, x) = reflect b r cAfter

-- prop_chooseAction_chooses_closest_classifier_model
--   :: TestBrain -> TestResponse -> Property
-- prop_chooseAction_chooses_closest_classifier_model b r
--   = property $ bmu == fst (minimumBy (comparing snd) rls)
--   where (cBMUs, lds, pBMU, rls, r, b') = chooseAction b ps c
--
-- prop_chooseAction_chooses_closest_predictor_model
--   :: TestBrain -> TestResponse -> Property
-- prop_chooseAction_chooses_closest_predictor_model b r
--   = property $ pBMU == fst (minimumBy (comparing snd) rls)
--   where (_, _, pBMU, rls, r, b') = chooseAction b ps c
--         model = (modelSet . _predictor $ b) ! pBMU
--         bmuDiff = diffIgnoringOutcome r model

data AFewPatterns = AFewPatterns [TestPattern]
  deriving (Eq, Show)

sizedArbAFewPatterns :: Int -> Gen AFewPatterns
sizedArbAFewPatterns n = do
  k <- choose (1, min 3 (n+1))
  AFewPatterns <$> vectorOf k arbitrary

instance Arbitrary AFewPatterns where
  arbitrary = sized sizedArbAFewPatterns

data ImprintTestData
  = ImprintTestData TestBrain [TestPattern] TestAction [UIDouble]
    deriving (Eq, Show)

sizedArbImprintTestData :: Int -> Gen ImprintTestData
sizedArbImprintTestData n = do
  cSize <- choose (1, max 1 n)
  nObjects <- choose (1, min 3 (max 1 (n - cSize)))
  nConditions <- choose (1, max 1 (n - cSize - nObjects))
  let pSize = max 1 (n - cSize - nObjects - nConditions)
  b <- arbTestBrain cSize nObjects nConditions pSize
  ps <- vectorOf nObjects arbitrary
  c <- vectorOf nConditions arbitrary
  a <- arbitrary
  return $ ImprintTestData b ps a c

instance Arbitrary ImprintTestData where
  arbitrary = sized sizedArbImprintTestData

prop_imprint_works
  :: ImprintTestData -> Property
prop_imprint_works (ImprintTestData b ps a c) = not (null ps) ==>
  _outcome rAfter >= _outcome rBefore
  where (_, lds, b') = assessSituation b ps
        (s, l) = head $ mostLikelyScenarios (_muser b') c lds
        r = Response s a 1
        ((rBefore, _):_, b2) = predictAll b' [(r, l)]
        b3 = imprint b2 ps a c
        ((rAfter, _):_, _) = predictAll b3 [(r, l)]

test :: Test
test = testGroup "ALife.Creatur.Wain.BrainQC"
  [
    testProperty "prop_serialize_round_trippable - Brain"
      (prop_serialize_round_trippable :: TestBrain -> Property),
    testProperty "prop_genetic_round_trippable - Brain"
      (prop_genetic_round_trippable equivBrain :: TestBrain -> Property),
    testProperty "prop_diploid_identity - Brain"
      (prop_diploid_identity equivBrain :: TestBrain -> Property),
    testProperty "prop_diploid_expressable - Brain"
      (prop_diploid_expressable :: TestBrain -> TestBrain -> Property),
    testProperty "prop_diploid_readable - Brain"
      (prop_diploid_readable :: TestBrain -> TestBrain -> Property),
    testProperty "prop_reflect_makes_predictions_more_accurate"
      prop_reflect_makes_predictions_more_accurate,
    testProperty "prop_reflect_error_in_range"
      prop_reflect_error_in_range,
    testProperty "prop_imprint_works"
      prop_imprint_works
  ]
