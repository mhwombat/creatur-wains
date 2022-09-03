------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.TeachingTest
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify that a wain can be taught.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import ALife.Creatur.Gene.Numeric.PlusMinusOne qualified as PM1
import ALife.Creatur.Gene.Numeric.UnitInterval qualified as UI
import ALife.Creatur.Gene.Numeric.Weights      (makeWeights)
import ALife.Creatur.Wain
import ALife.Creatur.Wain.BrainInternal        (classifier, decisionQuality,
                                                makeBrain, predictor)
import ALife.Creatur.Wain.GeneticSOM           (schemaQuality)
import ALife.Creatur.Wain.LearningParams       (mkLearningParams)
import ALife.Creatur.Wain.PatternQC            (TestPattern (..),
                                                TestPatternAdjuster (..))
import ALife.Creatur.Wain.ResponseQC           (TestAction (..),
                                                TestResponseAdjuster (..))
import ALife.Creatur.Wain.SimpleMuser          (SimpleMuser, makeMuser)
import ALife.Creatur.Wain.Statistics           (stats)
import Control.Monad                           (foldM_)
import Control.Monad.Random                    (evalRand, getRandoms, mkStdGen)
import Data.Datamining.Clustering.SGM4         qualified as SOM

reward :: Double
reward = 0.1

correctAnswer :: TestPattern -> TestAction
correctAnswer (TestPattern p)
  | p < 50    = Walk
  | p < 100   = Run
  | p < 150   = Jump
  | p < 200   = Skip
  | otherwise = Crawl

energyFor :: TestPattern -> TestAction -> Double
energyFor p a = if a == correctAnswer p then reward else -reward

type TestWain = Wain TestPatternAdjuster TestResponseAdjuster
                     TestPattern TestAction (SimpleMuser TestAction)

imprintAll :: TestWain -> TestWain
imprintAll w = fst . imprintResponse [TestPattern 25] Walk
                 . fst . imprintResponse [TestPattern 75] Run
                 . fst . imprintResponse [TestPattern 125] Jump
                 . fst . imprintResponse [TestPattern 175] Skip
                 . fst $ imprintResponse [TestPattern 225] Crawl w


testWain :: TestWain
testWain = w'
  where wName = "Fred"
        wAppearance = TestPattern 0
        (Right wBrain) = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1 128 wIos wRds
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wClassifierSize = 5
        wClassifierAdjuster = TestPatternAdjuster ec
        wClassifier = SOM.makeSGM wClassifierAdjuster wClassifierSize
        (Right wMuser) = makeMuser [0, 0, 0] 1
        wIos = [PM1.narrow reward, 0, 0]
        wRds = [PM1.narrow reward, 0, 0]
        wPredictorAdjuster = TestResponseAdjuster ep
        wPredictor = SOM.makeSGM wPredictorAdjuster (wClassifierSize*5)
        wHappinessWeights = makeWeights [1, 0, 0]
        Right ec = mkLearningParams 0.1 0.0001 1000
        -- This wain will be taught the correct actions up front.
        -- After storing those initial action models, it doesn't need to
        -- learn anything.
        Right ep = mkLearningParams 0.1 0.0001 1000
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta
        w' = fst $ adjustEnergy 0.5 "initial" w

tryOne :: TestWain -> (Int, TestPattern) -> IO TestWain
tryOne w (n, p) = do
  putStrLn $ "----- Round " ++ show n ++ " -----"
  putStrLn $ "stats=" ++ show (stats w)
  putStrLn "Initial classifier models"
  mapM_ putStrLn $ prettyClassifierModels w
  putStrLn "Initial decision models"
  mapM_ putStrLn $ prettyBrainSummary w
  let (wainAfterDecision, a, _) = chooseAction [p] w
  -- mapM_ putStrLn $ prettyClassificationReport wainAfterDecision report
  -- mapM_ putStrLn $ prettyScenarioReport wainAfterDecision report
  -- mapM_ putStrLn $ prettyPredictionReport wainAfterDecision report
  -- mapM_ putStrLn $ prettyActionReport wainAfterDecision report
  putStrLn $ "DEBUG classifier SQ=" ++ show (schemaQuality . classifier . brain $ wainAfterDecision)
  let deltaE = energyFor p a
  let wainRewarded = fst $ adjustEnergy deltaE "reward" wainAfterDecision
  putStrLn $ "Δe=" ++ show deltaE
  putStrLn $ "condition before=" ++ show (condition w) ++ " after=" ++ show (condition wainRewarded)
  putStrLn $ "happiness before=" ++ show (happiness w) ++ " after=" ++ show (happiness wainRewarded)
  -- let bmu = head . bmus . wdrClassifierReport $ report
  putStr $ "Choosing to " ++ show a ++ " in response to " ++ show p
  if deltaE < 0
    then putStrLn " was wrong"
    else putStrLn " was correct"
  let wainAfterReflection = fst $ reflect wainRewarded
  -- mapM_ putStrLn $ prettyReflectionReport wainAfterReflection rReflect
  -- keep the wain's energy constant
  let restorationEnergy = UI.wide (energy w) - UI.wide (energy wainAfterReflection)
  let wainFinal = fst $ adjustEnergy restorationEnergy "restoration" wainAfterReflection
  putStrLn "Final classifier models"
  mapM_ putStrLn $ prettyClassifierModels wainFinal
  putStrLn "Final decision models"
  mapM_ putStrLn $ prettyBrainSummary wainFinal
  putStrLn $ "classifier SQ=" ++ show (schemaQuality . classifier . brain $ w)
  putStrLn $ "predictor SQ=" ++ show (schemaQuality . predictor . brain $ w)
  putStrLn $ "DQ=" ++ show (decisionQuality . brain $ w)
  return wainFinal

main :: IO ()
main = do
  putStrLn "In the first round, the wain should have 5 classifier"
  putStrLn "models that are exactly 25, 75, 125, 175 and 225."
  putStrLn "In the first round, the predictor should have all the"
  putStrLn "correct actions."
  putStrLn "The classifier models should be unchanged at the end."
  putStrLn "The predictor models will vary, but the correct actions"
  putStrLn "should not change."
  putStrLn "The only mistakes the wain should make are for boundary"
  putStrLn "values (50, 100, 150, 200)."
  let g = mkStdGen 514229 -- seed
  let ps = map TestPattern . take 1000 $ evalRand getRandoms g
  let w = imprintAll testWain
  let nps = zip [1..] ps
  foldM_ tryOne w nps
  putStrLn "test complete"
