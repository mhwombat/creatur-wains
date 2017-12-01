------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.BrainQC
-- Copyright   :  (c) Amy de Buitléir 2013-2017
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
import ALife.Creatur.Wain.Probability (hypothesise)
import ALife.Creatur.Wain.Response (Response(..), _outcomes)
import ALife.Creatur.Wain.ResponseQC (TestAction, TestResponse,
  arbTestResponse)
import ALife.Creatur.Wain.SimpleMuser (SimpleMuser, makeMuser,
  _defaultOutcomes)
import ALife.Creatur.Wain.SimpleResponseTweaker (ResponseTweaker(..))
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.Weights (makeWeights)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type TestBrain = Brain TestPattern C.TestTweaker (ResponseTweaker TestAction) (SimpleMuser TestAction) TestAction

sizedArbTestBrain :: Int -> Gen TestBrain
sizedArbTestBrain n = do
  cSize <- choose (0, n)
  nObjects <- choose (0, n - cSize)
  -- nConditions <- choose (0, n - cSize - nObjects)
  let nConditions = 4
  let pSize = n + 1
  arbTestBrain cSize nObjects nConditions pSize

arbTestBrain :: Int -> Int -> Int -> Int -> Gen TestBrain
arbTestBrain cSize nObjects nConditions pSize = do
  c <- sizedArbGeneticSOM arbitrary cSize
  os <- vectorOf nConditions arbitrary
  d <- max 1 <$> arbitrary
  let (Right m) = makeMuser os d
  p <- D.arbTestPredictor nObjects nConditions pSize
  hw <- makeWeights <$> vectorOf nConditions arbitrary
  t <- arbitrary
  s <- choose (1, 255)
  ios <- vectorOf nConditions $ choose (-0.9999, 1)
  rds <- vectorOf nConditions $ choose (-0.9999, 1)
  let (Right b) = makeBrain c m p hw t s ios rds
  return b

-- Like arbTestBrain, except that imprint outcomes and reinforcement
-- deltas are positive
arbSensibleTestBrain :: Int -> Int -> Int -> Int -> Gen TestBrain
arbSensibleTestBrain cSize nObjects nConditions pSize = do
  c <- sizedArbGeneticSOM arbitrary cSize
  os <- vectorOf nConditions arbitrary
  d <- max 1 <$> arbitrary
  let (Right m) = makeMuser os d
  p <- D.arbTestPredictor nObjects nConditions pSize
  hw <- makeWeights <$> vectorOf nConditions arbitrary
  t <- arbitrary
  s <- choose (1, 255)
  ios <- vectorOf nConditions $ choose (0.0001, 1)
  rds <- vectorOf nConditions $ choose (0.0001, 1)
  let (Right b) = makeBrain c m p hw t s ios rds
  return b

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
  let (Right m) = makeMuser os d
  p <- D.arbEmptyTestPredictor pSize
  hw <- makeWeights <$> vectorOf nConditions arbitrary
  t <- arbitrary
  s <- choose (1, 255)
  ios <- vectorOf nConditions $ choose (0.01, 1)
  rds <- vectorOf nConditions $ choose (0.01, 1)
  let (Right b) = makeBrain c m p hw t s ios rds
  return b

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
  -- nConditions <- choose (0, n - cSize - nObjects)
  let nConditions = 4
  let pSize = n + 1
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
  where ((r2, _, _, _, _):_) = predictAll b . zip [r] $ repeat 1
        (b2, _, errBefore) = reflect b r2 cBefore cAfter
        ((r3, _, _, _, _):_) = predictAll b . zip [r] $ repeat 1
        (_, _, errAfter) = reflect b2 r3 cBefore cAfter

prop_reflect_error_in_range
  :: TestBrain -> TestResponse -> Condition -> Condition -> Property
prop_reflect_error_in_range b r cBefore cAfter
  = property $ -2 <= x && x <= 2
  where (_, _, x) = reflect b r cBefore cAfter

-- data AFewPatterns = AFewPatterns [TestPattern]
--   deriving (Eq, Show)

-- sizedArbAFewPatterns :: Int -> Gen AFewPatterns
-- sizedArbAFewPatterns n = do
--   k <- choose (1, min 3 (n+1))
--   AFewPatterns <$> vectorOf k arbitrary

-- instance Arbitrary AFewPatterns where
--   arbitrary = sized sizedArbAFewPatterns

data ImprintTestData
  = ImprintTestData TestBrain [TestPattern] TestAction Condition
    deriving Eq

sizedArbImprintTestData :: Int -> Gen ImprintTestData
sizedArbImprintTestData n = do
  cSize <- choose (1, max 1 n)
  nObjects <- choose (1, min 3 (max 1 (n - cSize)))
  -- nConditions <- choose (1, max 1 (n - cSize - nObjects))
  let nConditions = 4
  let pSize = n + 1
  b <- arbSensibleTestBrain cSize nObjects nConditions pSize
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
  where goodOutcomes = map (const 1) $ _imprintOutcomes b
        badOutcomes = map (const (-1)) $ _imprintOutcomes b
        m = _muser b
        mModified = m { _defaultOutcomes = badOutcomes }
        bModified = b { _imprintOutcomes = goodOutcomes,
                        _muser = mModified }
        (_, lds, bClassified) = classifyInputs bModified ps
        s = fst . maximumBy (comparing snd) . hypothesise (_strictness b) $ lds
        r = Response s a badOutcomes
        ((rBefore, _, _, _, _):_) = predictAll bClassified [(r, 1)]
        (_, _, _, _, bImprinted) = imprint bClassified ps a
        ((rAfter, _, _, _, _):_) = predictAll bImprinted [(r, 1)]

data ImprintEmptyBrainTestData
  = ImprintEmptyBrainTestData TestBrain [TestPattern] TestAction Condition
    deriving Eq

sizedArbImprintEmptyBrainTestData :: Int -> Gen ImprintEmptyBrainTestData
sizedArbImprintEmptyBrainTestData n = do
  cSize <- choose (1, max 1 n)
  nObjects <- choose (1, min 3 (max 1 (n - cSize)))
  -- nConditions <- choose (1, max 1 (n - cSize - nObjects))
  let nConditions = 4
  let pSize = n + 1
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
  where badOutcomes = map (const (-1)) $ _imprintOutcomes b
        bModified = b { _muser = mModified }
        mModified = (_muser b) { _defaultOutcomes = badOutcomes }
        (_, _, _, _, bImprinted) = imprint bModified ps a
        (_, _, _, _, r, _) = chooseAction bImprinted ps c

-- imprintAll :: TestBrain -> [([TestPattern], TestAction)] -> TestBrain
-- imprintAll b psas = foldl' imprintOne b psas

-- imprintOne :: TestBrain -> ([TestPattern], TestAction) -> TestBrain
-- imprintOne b (ps, a) = imprint b ps a

-- data ImprintTestData2
--   = ImprintTestData2 TestBrain [TestPattern] TestAction Condition String
--     deriving Eq

-- -- Are the two stimuli sufficiently similar that the brain might
-- -- classify them using the same response labels?
-- similar :: TestBrain -> [TestPattern] -> [TestPattern] -> Bool
-- similar b xs ys = xPLabel == yPLabel
--   where xPLabel = (f . classify p) xResponse :: Label
--         yPLabel = (f . classify p) yResponse :: Label
--         xResponse = Response xCLabels a [] :: Response TestAction
--         yResponse = Response yCLabels a [] :: Response TestAction
--         -- The next two lines should really use a fold
--         xCLabels = map (f . classify c) xs :: [Label]
--         yCLabels = map (f . classify c) ys :: [Label]
--         c = _classifier b
--         p = _predictor b
--         f (z, _, _, _) = z
--         a = minBound :: TestAction
        
-- sizedArbImprintTestData2 :: Int -> Gen ImprintTestData2
-- sizedArbImprintTestData2 n = do
--   nObjects <- min 3 <$> choose (1, n)
--   nConditions <- min 5 <$> choose (1, n+1)
--   nPrelearnedPatterns <- min 5 <$> choose (0, n)
--   -- Make sure the classifier has room to create new models if needed
--   cSize <- (nObjects*(nPrelearnedPatterns + 1) + 10 +) <$> choose (0, n)
--   -- Make sure the predictor has room to learn a new response
--   pSize <- (nPrelearnedPatterns + 10 +) <$> choose (0, n)
--   -- Generate a brain
--   b <- arbEmptyTestBrain cSize nConditions pSize
--   -- Generate some patterns for testing and pre-imprinting.
--   -- But make sure none of the responses we're going to teach the brain
--   -- are inconsistent; it will make testing difficult.
--   (ps:pss) <- nubBy (similar b) <$> vectorOf (nPrelearnedPatterns+1) (vectorOf nObjects arbitrary)
--   -- Pre-imprint the brain with some responses
--   as <- vectorOf nPrelearnedPatterns arbitrary
--   let psas = zip pss as
--   let bImprinted = imprintAll b psas
--   -- generate some extra test data
--   a <- arbitrary
--   c <- vectorOf nConditions arbitrary
--   return $ ImprintTestData2 bImprinted ps a c (show psas)

-- instance Arbitrary ImprintTestData2 where
--   arbitrary = sized sizedArbImprintTestData2

-- instance Show ImprintTestData2 where
--   show (ImprintTestData2 b ps a c imprintingInfo)
--     = "ImprintTestData2 (" ++ show b ++ ") " ++ show ps ++ " "
--       ++ show a ++ " " ++ show c ++ " \"" ++ imprintingInfo ++ "\""

-- prop_classifier_behaves_like_sgm
--   :: TestBrain -> [TestPattern] -> TestAction -> Property
-- prop_classifier_behaves_like_sgm b ps a =
--   property $ bModels == sModels
--   where dummy = view classifier b :: C.TestClassifier
--         s = view (patternMap . classifier) b :: SOM.SGM Word16 UIDouble Label TestPattern
--         b' = imprint b ps a
--         (_, _, _, s')  = SOM.trainAndClassify s ps
--         bModels = M.elems . modelMap . view classifier $ b'
--         sModels = M.elems . SOM.modelMap $ s'

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
