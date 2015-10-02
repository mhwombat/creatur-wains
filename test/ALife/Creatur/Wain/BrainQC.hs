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
import ALife.Creatur.Wain.GeneticSOMQC (sizedArbGeneticSOM,
  sizedArbEmptyGeneticSOM)
import ALife.Creatur.Wain.Muser (makeMuser, _defaultOutcomes)
import ALife.Creatur.Wain.Response (Response(..), _outcomes)
import ALife.Creatur.Wain.ResponseQC (TestAction, TestResponse,
  arbTestResponse)
import ALife.Creatur.Wain.Probability (hypothesise)
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.Weights (makeWeights)
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
  os <- vectorOf nConditions arbitrary
  d <- max 1 <$> arbitrary
  let m = makeMuser os d
  p <- D.arbTestPredictor nObjects nConditions pSize
  hw <- makeWeights <$> vectorOf nConditions arbitrary
  t <- arbitrary
  ios <- vectorOf nConditions $ choose (-0.99999, 1)
  return $ makeBrain c m p hw t ios

instance Arbitrary TestBrain where
  arbitrary = sized sizedArbTestBrain

equivBrain :: TestBrain -> TestBrain -> Bool
equivBrain b1 b2 = _classifier b1 `C.equivClassifier` _classifier b2
  && _predictor b1 `D.equivPredictor` _predictor b2

-- sizedArbEmptyTestBrain :: Int -> Gen TestBrain
-- sizedArbEmptyTestBrain n = do
--   cSize <- choose (0, n)
--   nObjects <- choose (0, n - cSize)
--   nConditions <- choose (0, n - cSize - nObjects)
--   let pSize = n - cSize - nObjects - nConditions
--   arbTestBrain cSize nObjects nConditions pSize

arbEmptyTestBrain :: Int -> Int -> Int -> Gen TestBrain
arbEmptyTestBrain cSize nConditions pSize = do
  c <- sizedArbEmptyGeneticSOM cSize
  os <- vectorOf nConditions arbitrary
  d <- max 1 <$> arbitrary
  let m = makeMuser os d
  p <- D.arbEmptyTestPredictor pSize
  hw <- makeWeights <$> vectorOf nConditions arbitrary
  t <- arbitrary
  ios <- vectorOf nConditions $ choose (0.01, 1)
  return $ makeBrain c m p hw t ios

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
  where ((r2, _, _, _):_) = predictAll b . zip [r] $ repeat 1
        (b2, errBefore) = reflect b r2 cBefore cAfter
        ((r3, _, _, _):_) = predictAll b . zip [r] $ repeat 1
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
  = ImprintTestData TestBrain [TestPattern] TestAction Condition
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
  c <- vectorOf nConditions arbitrary
  return $ ImprintTestData b ps a c

instance Arbitrary ImprintTestData where
  arbitrary = sized sizedArbImprintTestData

instance Show ImprintTestData where
  show (ImprintTestData b ps a c)
    = "ImprintTestData (" ++ show b ++ ") " ++ show ps ++ " "
      ++ show a ++ " " ++ show c

prop_imprint_works :: ImprintTestData -> Property
prop_imprint_works (ImprintTestData b ps a _) = not (null ps)
    ==> and $ zipWith (>=) (_outcomes rAfter) (_outcomes rBefore)
  where bModified = b { _imprintOutcomes = replicate nConditions 1 }
        (_, lds, bClassified) = classifyInputs bModified ps
        s = fst . maximumBy (comparing snd) . hypothesise $ lds
        r = Response s a $ replicate nConditions 1
        ((rBefore, _, _, _):_) = predictAll bClassified [(r, 1)]
        bImprinted = imprint bClassified ps a
        ((rAfter, _, _, _):_) = predictAll bImprinted [(r, 1)]
        nConditions = length $ _imprintOutcomes b

data ImprintEmptyBrainTestData
  = ImprintEmptyBrainTestData TestBrain [TestPattern] TestAction Condition
    deriving Eq

sizedArbImprintEmptyBrainTestData :: Int -> Gen ImprintEmptyBrainTestData
sizedArbImprintEmptyBrainTestData n = do
  cSize <- choose (1, max 1 n)
  nObjects <- choose (1, min 3 (max 1 (n - cSize)))
  nConditions <- choose (1, max 1 (n - cSize - nObjects))
  let pSize = max 1 (n - cSize - nObjects - nConditions)
  b <- arbEmptyTestBrain cSize nConditions pSize
  ps <- vectorOf nObjects arbitrary
  a <- arbitrary
  c <- vectorOf nConditions arbitrary
  return $ ImprintEmptyBrainTestData b ps a c

instance Arbitrary ImprintEmptyBrainTestData where
  arbitrary = sized sizedArbImprintEmptyBrainTestData

instance Show ImprintEmptyBrainTestData where
  show (ImprintEmptyBrainTestData b ps a c)
    = "ImprintEmptyBrainTestData (" ++ show b ++ ") " ++ show ps ++ " "
      ++ show a ++ " " ++ show c

prop_imprint_works2 :: ImprintEmptyBrainTestData -> Property
prop_imprint_works2 (ImprintEmptyBrainTestData b ps a c) = not (null ps)
  ==> _action r == a
  where bModified = b { _muser = mModified }
        mModified = (_muser b)
                      { _defaultOutcomes = replicate nConditions (-1) }
        nConditions = length $ _imprintOutcomes b
        bImprinted = imprint bModified ps a
        (_, _, _, _, r, _) = chooseAction bImprinted ps c

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
      prop_imprint_works,
    testProperty "prop_imprint_works2"
      prop_imprint_works2
  ]
