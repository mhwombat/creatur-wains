------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.BrainQC
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
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
    ReflectionTestData(..),
    ImprintTestData(..),
  ) where

import ALife.Creatur.Gene.Numeric.PlusMinusOne qualified as PM1
import ALife.Creatur.Gene.Numeric.UnitInterval qualified as UI
import ALife.Creatur.Gene.Numeric.Weights      (Weights, extractWeights,
                                                makeWeights, numWeights)
import ALife.Creatur.Gene.Test                 qualified as GT
import ALife.Creatur.Wain.BrainInternal
import ALife.Creatur.Wain.Classifier           qualified as Cl
import ALife.Creatur.Wain.GeneticSOM           (Label)
import ALife.Creatur.Wain.GeneticSOMQC         (arbGeneticSOM)
import ALife.Creatur.Wain.PatternQC            (TestPattern,
                                                TestPatternAdjuster)
import ALife.Creatur.Wain.Predictor            qualified as P
import ALife.Creatur.Wain.PredictorQC          qualified as PQC
import ALife.Creatur.Wain.Response             (Response (..))
import ALife.Creatur.Wain.ResponseQC           (TestAction, TestResponse,
                                                TestResponseAdjuster)
import ALife.Creatur.Wain.SimpleMuser          (SimpleMuser)
import ALife.Creatur.Wain.SimpleMuserQC        (sizedArbMuser)
import Control.DeepSeq                         (deepseq)
import Data.Datamining.Clustering.SGM4         qualified as SOM
import Numeric.ApproxEq                        qualified as EQ
import Test.Framework                          (Test, testGroup)
import Test.Framework.Providers.QuickCheck2    (testProperty)
import Test.QuickCheck.Counterexamples

-- Both get and express (re)normalise weights, which can result in
-- small differences even if the weights are already normalised.
equivWeights :: Weights Double -> Weights Double -> Bool
equivWeights x y
  = numWeights x == numWeights y
      && and (zipWith (EQ.within 100000) wx wy)
  where wx = extractWeights x
        wy = extractWeights y

equivBrain :: (Eq ct, Eq pt, Eq p, Eq a, Eq m) => Brain ct pt p a m -> Brain ct pt p a m -> Bool
equivBrain x y = x == y' && equivWeights wx wy
  where wx = happinessWeights x
        wy = happinessWeights y
        y' = y { happinessWeights=wx }

type TestBrain = Brain TestPatternAdjuster TestResponseAdjuster
                       TestPattern TestAction (SimpleMuser TestAction)

arbTestBrain :: Int -> Int -> Gen TestBrain
arbTestBrain nObjects nConditions = do
  c <- arbGeneticSOM arbitrary
  m <- sizedArbMuser nConditions
  p <- PQC.sizedArbTestPredictor nObjects nConditions
  hw <- resize nConditions arbitrary
  t <- arbitrary
  s <- choose (1, 255)
  ios <- vectorOf nConditions $ choose (-0.9999, 1)
  rds <- vectorOf nConditions $ choose (-0.9999, 1)
  let (Right b) = makeBrain c m p hw t s ios rds
  return b

-- Like arbTestBrain, except that imprint outcomes and reinforcement
-- deltas are positive
arbSensibleTestBrain :: Int -> Int -> Gen TestBrain
arbSensibleTestBrain nObjects nConditions = do
  c <- arbGeneticSOM arbitrary
  m <- sizedArbMuser nConditions
  p <- PQC.sizedArbTestPredictor nObjects nConditions
  hw <- resize nConditions arbitrary
  t <- arbitrary
  s <- choose (1, 255)
  ios <- vectorOf nConditions $ choose (0.0001, 1)
  rds <- vectorOf nConditions $ choose (0.0001, 1)
  let (Right b) = makeBrain c m p hw t s ios rds
  return b

instance Arbitrary TestBrain where
  arbitrary = do
    nObjects <- fmap (\n -> min 3 (n+1)) getSize
    let nConditions = 3
    arbTestBrain nObjects nConditions

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
  = ChoosingTestData TestBrain [TestPattern] Condition

instance Show ChoosingTestData where
  show (ChoosingTestData b ps c)
    = "ChoosingTestData (" ++ show b ++ ") " ++ show ps ++ " "
      ++ show c

instance Arbitrary ChoosingTestData where
  arbitrary = do
    nObjects <- fmap (\n -> min 3 (n+1)) getSize
    let nConditions = 3
    b <- arbSensibleTestBrain nObjects nConditions
    ps <- vectorOf nObjects arbitrary
    c <- vectorOf nConditions arbitrary
    return $ ChoosingTestData b ps c


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

instance Arbitrary ReflectionTestData where
  arbitrary = do
    nObjects <- fmap (\n -> min 3 (n+1)) getSize
    let nConditions = 3
    c <- vectorOf nConditions arbitrary
    ps <- vectorOf nObjects arbitrary
    b <- arbTestBrain nObjects nConditions
    let (report, b') = chooseAction b ps c
    let r = bdrRecommendedResponse report
    cBefore <- vectorOf nConditions arbitrary
    cAfter <- vectorOf nConditions arbitrary
    return $ ReflectionTestData b' r cBefore cAfter

prop_reflect_makes_predictions_more_accurate
  :: ReflectionTestData -> Bool
prop_reflect_makes_predictions_more_accurate
  (ReflectionTestData b _ _ cAfter)
    = errAfter <= errBefore
  where (Just report2, b2) = reflect b cAfter
        errBefore = brrErr report2
        b3 = b2 { lastChoice = lastChoice b } -- hack for testing
        (Just report4, _) = reflect b3 cAfter
        errAfter = brrErr report4

prop_reflect_error_in_range :: ReflectionTestData -> Bool
prop_reflect_error_in_range (ReflectionTestData b _ _ cAfter)
  = -2 <= x && x <= 2
  where (Just report, _) = reflect b cAfter
        x = brrErr report

prop_reflect_never_causes_error :: ReflectionTestData -> Bool
prop_reflect_never_causes_error (ReflectionTestData b _ _ cAfter)
  = deepseq x True
  where x = reflect b cAfter

-- data AFewPatterns = AFewPatterns [TestPattern]
--   deriving (Eq, Show)

-- sizedArbAFewPatterns :: Int -> Gen AFewPatterns
-- sizedArbAFewPatterns n = do
--   k <- choose (1, min 3 (n+1))
--   AFewPatterns <$> vectorOf k arbitrary

-- instance Arbitrary AFewPatterns where
--   arbitrary = sized sizedArbAFewPatterns

data ImprintTestData
  = ImprintTestData TestBrain [TestPattern] TestAction [PM1.Double] [Label]
    deriving Eq

instance Arbitrary ImprintTestData where
  arbitrary = do
    nObjects <- fmap (\n -> min 3 (n+1)) getSize
    let nConditions = 3
    b <- arbSensibleTestBrain nObjects nConditions
    ps <- vectorOf nObjects arbitrary
    a <- arbitrary
    os <- vectorOf nConditions arbitrary
    ls <- vectorOf (length ps) arbitrary
    return $ ImprintTestData b ps a os ls

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
        responseAdj = SOM.adjuster $ predictor b
        diffBefore = SOM.difference responseAdj rBefore r
        diffAfter = SOM.difference responseAdj rAfter r

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
