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
import ALife.Creatur.Wain.MuserQC ()
import ALife.Creatur.Wain.Response (Response(..), _outcomes)
import ALife.Creatur.Wain.ResponseQC (TestAction, TestResponse,
  arbTestResponse)
import ALife.Creatur.Wain.Probability (hypothesise)
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.WeightsQC ()
import Data.List (maximumBy)
import Data.Ord (comparing)
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
  m <- arbitrary
  p <- D.arbTestPredictor nObjects nConditions pSize
  hw <- arbitrary
  return $ makeBrain c m p hw

instance Arbitrary TestBrain where
  arbitrary = sized sizedArbTestBrain

equivBrain :: TestBrain -> TestBrain -> Bool
equivBrain b1 b2 = _classifier b1 `C.equivClassifier` _classifier b2
  && _predictor b1 `D.equivPredictor` _predictor b2

data ReflectionTestData
  = ReflectionTestData TestBrain TestResponse Condition Condition

instance Show ReflectionTestData where
  show (ReflectionTestData b r cBefore cAfter)
    = "ReflectionTestData (" ++ show b ++ ") (" ++ show r ++ ") "
      ++ show cBefore ++ " " ++ show cAfter

sizedArbReflectionTestData :: Int -> Gen ReflectionTestData
sizedArbReflectionTestData n = do
  cSize <- choose (0, n)
  nObjects <- choose (0, n - cSize)
  nConditions <- choose (0, n - cSize - nObjects)
  let pSize = n - cSize - nObjects - nConditions
  b <- arbTestBrain cSize nObjects nConditions pSize
  r <- arbTestResponse nObjects nConditions
  cBefore <- vectorOf nConditions arbitrary
  cAfter <- vectorOf nConditions arbitrary
  return $ ReflectionTestData b r cBefore cAfter

instance Arbitrary ReflectionTestData where
  arbitrary = sized sizedArbReflectionTestData

prop_reflect_makes_predictions_more_accurate
  :: ReflectionTestData -> Property
prop_reflect_makes_predictions_more_accurate
  (ReflectionTestData b r cBefore cAfter)
    = property $ errAfter <= errBefore
  where ((r2, _, _, _):_, _) = predictAll b . zip [r] $ repeat 1
        (b2, errBefore) = reflect b r2 cBefore cAfter
        ((r3, _, _, _):_, _) = predictAll b . zip [r] $ repeat 1
        (_, errAfter) = reflect b2 r3 cBefore cAfter

prop_reflect_error_in_range
  :: TestBrain -> TestResponse -> Condition -> Condition -> Property
prop_reflect_error_in_range b r cBefore cAfter
  = property $ -2 <= x && x <= 2
  where (_, x) = reflect b r cBefore cAfter

data AFewPatterns = AFewPatterns [TestPattern]
  deriving (Eq, Show)

sizedArbAFewPatterns :: Int -> Gen AFewPatterns
sizedArbAFewPatterns n = do
  k <- choose (1, min 3 (n+1))
  AFewPatterns <$> vectorOf k arbitrary

instance Arbitrary AFewPatterns where
  arbitrary = sized sizedArbAFewPatterns

data ImprintTestData
  = ImprintTestData TestBrain [TestPattern] TestAction
    deriving Eq

sizedArbImprintTestData :: Int -> Gen ImprintTestData
sizedArbImprintTestData n = do
  cSize <- choose (1, max 1 n)
  nObjects <- choose (1, min 3 (max 1 (n - cSize)))
  nConditions <- choose (1, max 1 (n - cSize - nObjects))
  let pSize = max 1 (n - cSize - nObjects - nConditions)
  b <- arbTestBrain cSize nObjects nConditions pSize
  ps <- vectorOf nObjects arbitrary
  a <- arbitrary
  return $ ImprintTestData b ps a

instance Arbitrary ImprintTestData where
  arbitrary = sized sizedArbImprintTestData

instance Show ImprintTestData where
  show (ImprintTestData b ps a)
    = "ImprintTestData (" ++ show b ++ ") " ++ show ps ++ " "
      ++ show a

prop_imprint_works
  :: ImprintTestData -> Int -> Property
prop_imprint_works (ImprintTestData b ps a) nConditions =
  not (null ps)
    ==> and $ zipWith (>=) (_outcomes rAfter) (_outcomes rBefore)
  where (_, lds, b') = classifyInputs b ps
        s = fst . maximumBy (comparing snd) . hypothesise $ lds
        r = Response s a $ replicate nConditions 1
        ((rBefore, _, _, _):_, b2) = predictAll b' [(r, 1)]
        b3 = imprint b2 ps a
        ((rAfter, _, _, _):_, _) = predictAll b3 [(r, 1)]

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
