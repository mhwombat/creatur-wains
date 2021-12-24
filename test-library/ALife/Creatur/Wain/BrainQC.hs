------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.BrainQC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.BrainQC
  (
    test,
    equivBrain,
    ChoosingTestData(..),
    sizedArbChoosingTestData,
    ReflectionTestData(..),
    sizedArbReflectionTestData,
    ImprintTestData(..),
    sizedArbImprintTestData
  ) where

import           ALife.Creatur.Gene.Numeric.PlusMinusOne  (PM1Double)
import           ALife.Creatur.Gene.Numeric.Weights       (makeWeights)
import qualified ALife.Creatur.Gene.Test                  as GT
import           ALife.Creatur.Wain.BrainInternal
import qualified ALife.Creatur.Wain.Classifier            as Cl
import qualified ALife.Creatur.Wain.ClassifierQC          as CQC
import           ALife.Creatur.Wain.GeneticSOMQC          (equivGSOM,
                                                           sizedArbGeneticSOM)
import qualified ALife.Creatur.Wain.Predictor             as P
import qualified ALife.Creatur.Wain.PredictorQC           as PQC
import           ALife.Creatur.Wain.Response              (Response (..))
import           ALife.Creatur.Wain.ResponseQC            (TestAction,
                                                           TestResponse)
import           ALife.Creatur.Wain.SimpleMuser           (SimpleMuser,
                                                           makeMuser)
import           ALife.Creatur.Wain.SimpleResponseTweaker (ResponseTweaker (..),
                                                           responseDiff)
import           Control.DeepSeq                          (deepseq)
import           Test.Framework                           (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2     (testProperty)
import           Test.QuickCheck

type TestBrain
  = Brain GT.TestPattern CQC.TestClassifierTweaker (ResponseTweaker TestAction) (SimpleMuser TestAction) TestAction

sizedArbTestBrain :: Int -> Gen TestBrain
sizedArbTestBrain n = do
  ~[cSize, nObjects] <- GT.divvy n 2
  let nConditions = 4
  let pSize = n + 1
  arbTestBrain cSize nObjects nConditions pSize

arbTestBrain :: Int -> Int -> Int -> Int -> Gen TestBrain
arbTestBrain cSize nObjects nConditions pSize = do
  c <- sizedArbGeneticSOM arbitrary cSize
  os <- vectorOf nConditions arbitrary
  d <- max 1 <$> arbitrary
  let (Right m) = makeMuser os d
  p <- PQC.arbTestPredictor nObjects nConditions pSize
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
  p <- PQC.arbTestPredictor nObjects nConditions pSize
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
equivBrain b1 b2 = _classifier b1 `equivGSOM` _classifier b2
  && _predictor b1 `equivGSOM` _predictor b2

-- sizedArbEmptyTestBrain :: Int -> Gen TestBrain
-- sizedArbEmptyTestBrain n = do
--   cSize <- choose (0, n)
--   nObjects <- choose (0, n - cSize)
--   nConditions <- choose (0, n - cSize - nObjects)
--   let pSize = n - cSize - nObjects - nConditions
--   arbTestBrain cSize nObjects nConditions pSize

-- arbEmptyTestBrain :: Int -> Int -> Int -> Gen TestBrain
-- arbEmptyTestBrain cSize nConditions pSize = do
--   c <- sizedArbEmptyGeneticSOM cSize
--   os <- vectorOf nConditions arbitrary
--   d <- max 1 <$> arbitrary
--   let (Right m) = makeMuser os d
--   p <- PQC.arbEmptyTestPredictor pSize
--   hw <- makeWeights <$> vectorOf nConditions arbitrary
--   t <- arbitrary
--   s <- choose (1, 255)
--   ios <- vectorOf nConditions $ choose (0.01, 1)
--   rds <- vectorOf nConditions $ choose (0.01, 1)
--   let (Right b) = makeBrain c m p hw t s ios rds
--   return b

data ChoosingTestData
  = ChoosingTestData TestBrain [GT.TestPattern] Condition

instance Show ChoosingTestData where
  show (ChoosingTestData b ps c)
    = "ChoosingTestData (" ++ show b ++ ") " ++ show ps ++ " "
      ++ show c

sizedArbChoosingTestData :: Int -> Gen ChoosingTestData
sizedArbChoosingTestData n = do
  ~[cSize, nObjects] <- GT.divvy (min 10 n) 2
  -- nConditions <- choose (0, n - cSize - nObjects)
  let nConditions = 4
  let pSize = n + 1
  b <- arbSensibleTestBrain cSize nObjects nConditions pSize
  ps <- vectorOf nObjects arbitrary
  c <- vectorOf nConditions arbitrary
  return $ ChoosingTestData b ps c

instance Arbitrary ChoosingTestData where
  arbitrary = sized sizedArbChoosingTestData

-- prop_chooseAction_doesnt_add_predictor_models
--   :: ChoosingTestData -> Bool
-- prop_chooseAction_doesnt_add_predictor_models (ChoosingTestData b ps c)
--   = n' == n
--   where n = numModels . _predictor $ b
--         (_, b') = chooseAction b ps c
--         n' = numModels . _predictor $ b'

prop_chooseAction_never_causes_error
  :: ChoosingTestData -> Bool
prop_chooseAction_never_causes_error (ChoosingTestData b ps c)
  = deepseq x True
  where x = chooseAction b ps c

data ReflectionTestData
  = ReflectionTestData TestBrain TestResponse Condition Condition

instance Show ReflectionTestData where
  show (ReflectionTestData b r cBefore cAfter)
    = "ReflectionTestData (" ++ show b ++ ") (" ++ show r ++ ") "
      ++ show cBefore ++ " " ++ show cAfter

sizedArbReflectionTestData :: Int -> Gen ReflectionTestData
sizedArbReflectionTestData n = do
  ~[cSize, nObjects] <- GT.divvy (min 10 n) 2
  let nConditions = 4
  c <- vectorOf nConditions arbitrary
  ps <- vectorOf nObjects arbitrary
  let pSize = n + 1
  b <- arbTestBrain cSize nObjects nConditions pSize
  let (report, b') = chooseAction b ps c
  let r = bdrRecommendedResponse report
  cBefore <- vectorOf nConditions arbitrary
  cAfter <- vectorOf nConditions arbitrary
  return $ ReflectionTestData b' r cBefore cAfter

instance Arbitrary ReflectionTestData where
  arbitrary = sized sizedArbReflectionTestData

prop_reflect_makes_predictions_more_accurate
  :: ReflectionTestData -> Bool
prop_reflect_makes_predictions_more_accurate
  (ReflectionTestData b r cBefore cAfter)
    = errAfter <= errBefore
  where (report:_) = predictAll b . zip [r] $ repeat 1
        r2 = P.pResponse report
        (report2, b2) = reflect b r2 cBefore cAfter
        errBefore = brrErr report2
        (report3:_) = predictAll b . zip [r] $ repeat 1
        r3 = P.pResponse report3
        (report4, _) = reflect b2 r3 cBefore cAfter
        errAfter = brrErr report4

prop_reflect_error_in_range
  :: TestBrain -> TestResponse -> Condition -> Condition -> Bool
prop_reflect_error_in_range b r cBefore cAfter = -2 <= x && x <= 2
  where (report, _) = reflect b r cBefore cAfter
        x = brrErr report

prop_reflect_never_causes_error
  :: ReflectionTestData -> Bool
prop_reflect_never_causes_error (ReflectionTestData b r cBefore cAfter)
  = deepseq x True
  where x = reflect b r cBefore cAfter

-- data AFewPatterns = AFewPatterns [GT.TestPattern]
--   deriving (Eq, Show)

-- sizedArbAFewPatterns :: Int -> Gen AFewPatterns
-- sizedArbAFewPatterns n = do
--   k <- choose (1, min 3 (n+1))
--   AFewPatterns <$> vectorOf k arbitrary

-- instance Arbitrary AFewPatterns where
--   arbitrary = sized sizedArbAFewPatterns

data ImprintTestData
  = ImprintTestData TestBrain [GT.TestPattern] TestAction [PM1Double] [Cl.Label]
    deriving Eq

sizedArbImprintTestData :: Int -> Gen ImprintTestData
sizedArbImprintTestData n = do
  ~[cSize, nO] <- GT.divvy (min 10 n) 2
  let nObjects = min 3 nO
  let nConditions = 4
  let pSize = n + 1
  b <- arbSensibleTestBrain cSize nObjects nConditions pSize
  ps <- vectorOf nObjects arbitrary
  a <- arbitrary
  os <- vectorOf nConditions arbitrary
  ls <- vectorOf (length ps) arbitrary
  return $ ImprintTestData b ps a os ls

instance Arbitrary ImprintTestData where
  arbitrary = sized sizedArbImprintTestData

instance Show ImprintTestData where
  show (ImprintTestData b ps a os ls)
    = "ImprintTestData (" ++ show b ++ ") " ++ show ps ++ " "
      ++ show a ++ " " ++ show os ++ " " ++ show ls

prop_imprintStimulus_never_causes_error
  :: ImprintTestData -> Bool
prop_imprintStimulus_never_causes_error (ImprintTestData b ps _ _ ls)
  = deepseq (imprintStimulus b lps) True
  where lps = zip ls ps

prop_imprintResponse_never_causes_error
  :: ImprintTestData -> Bool
prop_imprintResponse_never_causes_error (ImprintTestData b ps a _ _)
  = deepseq x True
  where (report1, bClassified) = classifyInputs b ps
        ls = Cl.bmus report1
        x = imprintResponse bClassified ls a

prop_imprint_makes_predictions_more_accurate
  :: ImprintTestData -> Bool
prop_imprint_makes_predictions_more_accurate
  (ImprintTestData b ps a os _)
    = diffAfter <= diffBefore
  where (report1, bClassified) = classifyInputs b ps
        ls = Cl.bmus report1
        r = Response ls a os
        (report2:_) = predictAll bClassified . zip [r] $ repeat 1
        rBefore = P.pResponse report2
        (_, bImprinted) = imprintResponse bClassified ls a
        (report3:_) = predictAll bImprinted . zip [r] $ repeat 1
        rAfter = P.pResponse report3
        diffBefore = responseDiff rBefore r
        diffAfter = responseDiff rAfter r

-- prop_prettyScenarioReport_never_causes_error
--   :: ChoosingTestData -> Bool
-- prop_prettyScenarioReport_never_causes_error
--   (ChoosingTestData b ps c)
--   = deepseq x' True
--   where (x, _) = chooseAction b ps c
--         x' = prettyScenarioReport $ bdrScenarioReport x

-- prop_prettyActionReport_never_causes_error
--   :: ChoosingTestData -> Bool
-- prop_prettyActionReport_never_causes_error
--   (ChoosingTestData b ps c)
--   = deepseq x' True
--   where (x, _) = chooseAction b ps c
--         x' = prettyActionReport $ bdrActionReport x

-- prop_prettyReflectionReport_never_causes_error
--   :: ReflectionTestData -> Bool
-- prop_prettyReflectionReport_never_causes_error
--   (ReflectionTestData b r cBefore cAfter)
--   = deepseq x' True
--   where (x, _) = reflect b r cBefore cAfter
--         x' = prettyReflectionReport x

-- prop_prettyImprintReport_never_causes_error
--   :: ImprintTestData -> Bool
-- prop_prettyImprintReport_never_causes_error
--   (ImprintTestData b ps a _)
--   = deepseq x' True
--   where (x, _) = imprint b ps a
--         x' = prettyImprintReport x

test :: Test
test = testGroup "ALife.Creatur.Wain.BrainQC"
  [
    testProperty "prop_serialize_round_trippable - Brain"
      (GT.prop_serialize_round_trippable :: TestBrain -> Bool),
    testProperty "prop_genetic_round_trippable - Brain"
      (GT.prop_genetic_round_trippable equivBrain :: TestBrain -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - Brain"
    --   (GT.prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestBrain -> Bool),
    testProperty "prop_diploid_identity - Brain"
      (GT.prop_diploid_identity equivBrain :: TestBrain -> Bool),
    -- testProperty "prop_show_read_round_trippable - Brain"
    --   (GT.prop_show_read_round_trippable (==) :: TestBrain -> Bool),
    testProperty "prop_diploid_expressable - Brain"
      (GT.prop_diploid_expressable :: TestBrain -> TestBrain -> Bool),
    testProperty "prop_diploid_readable - Brain"
      (GT.prop_diploid_readable :: TestBrain -> TestBrain -> Bool),
    -- testProperty "prop_chooseAction_doesnt_add_predictor_models"
    --   prop_chooseAction_doesnt_add_predictor_models,
    testProperty "prop_chooseAction_never_causes_error"
      prop_chooseAction_never_causes_error,
    testProperty "prop_reflect_makes_predictions_more_accurate"
      prop_reflect_makes_predictions_more_accurate,
    testProperty "prop_reflect_error_in_range"
      prop_reflect_error_in_range,
    testProperty "prop_reflect_never_causes_error"
      prop_reflect_never_causes_error,
    testProperty "prop_imprintStimulus_never_causes_error"
      prop_imprintStimulus_never_causes_error,
    testProperty "prop_imprintResponse_never_causes_error"
      prop_imprintResponse_never_causes_error,
    testProperty "prop_imprint_makes_predictions_more_accurate"
      prop_imprint_makes_predictions_more_accurate
    -- testProperty "prop_prettyScenarioReport_never_causes_error"
    --   prop_prettyScenarioReport_never_causes_error,
    -- testProperty "prop_prettyActionReport_never_causes_error"
    --   prop_prettyActionReport_never_causes_error,
    -- testProperty "prop_prettyReflectionReport_never_causes_error"
    --   prop_prettyReflectionReport_never_causes_error,
    -- testProperty "prop_prettyImprintReport_never_causes_error"
    --   prop_prettyImprintReport_never_causes_error
  ]
