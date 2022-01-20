------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.LearningTest
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify that a wain can learn from experience.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import qualified ALife.Creatur.Gene.Numeric.PlusMinusOne as PM1
import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import           ALife.Creatur.Gene.Numeric.Weights      (makeWeights)
import           ALife.Creatur.Wain.BrainInternal        (classifier,
                                                          decisionQuality,
                                                          makeBrain, predictor)
import           ALife.Creatur.Wain.GeneticSOM           (schemaQuality)
import           ALife.Creatur.Wain.LearningParams       (mkLearningParams)
import           ALife.Creatur.Wain.PatternQC            (TestPattern (..),
                                                          TestPatternAdjuster (..))
import           ALife.Creatur.Wain.ResponseQC           (TestAction (..),
                                                          TestResponseAdjuster (..))
import           ALife.Creatur.Wain.SimpleMuser          (SimpleMuser,
                                                          makeMuser)
import           ALife.Creatur.Wain.Statistics           (stats)
import           ALife.Creatur.WainInternal
import           Control.Monad                           (foldM_)
import           Control.Monad.Random                    (evalRand, getRandoms,
                                                          mkStdGen)
import qualified Data.Datamining.Clustering.SGM4         as SOM

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

testWain :: TestWain
testWain = w'
  where wName = "Fred"
        wAppearance = TestPattern 0
        (Right wBrain) = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1 16 wIos wRds
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wClassifierSize = 10
        wClassifierAdjuster = TestPatternAdjuster ec
        wClassifier = SOM.makeSGM wClassifierAdjuster wClassifierSize
        (Right wMuser) = makeMuser [0, 0, 0] 3
        wIos = [PM1.narrow reward, 0, 0]
        wRds = [PM1.narrow reward, 0, 0]
        wPredictorAdjuster = TestResponseAdjuster ep
        wPredictor = SOM.makeSGM wPredictorAdjuster (wClassifierSize*5)
        wHappinessWeights = makeWeights [1, 0, 0]
        Right ec = mkLearningParams 0.1 0.0001 1000
        Right ep = mkLearningParams 0.1 0.0001 1000
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta
        w' = adjustEnergy 0.5 "initial" w

tryOne :: TestWain -> (Int, TestPattern) -> IO TestWain
tryOne w (n, p) = do
  putStrLn $ "----- Round " ++ show n ++ " -----"
  putStrLn $ "stats=" ++ show (stats w)
  putStrLn "Initial classifier models"
  mapM_ putStrLn $ prettyClassifierModels w
  putStrLn "Initial decision models"
  mapM_ putStrLn $ prettyBrainSummary w
  let (wainAfterDecision, a) = chooseAction [p] w
  putStrLn "Interim classifier models"
  mapM_ putStrLn $ prettyClassifierModels wainAfterDecision
  -- mapM_ putStrLn $ prettyClassificationReport wainAfterDecision report
  -- mapM_ putStrLn $ prettyScenarioReport wainAfterDecision report
  -- mapM_ putStrLn $ prettyPredictionReport wainAfterDecision report
  -- mapM_ putStrLn $ prettyActionReport wainAfterDecision report
  putStrLn $ "DEBUG classifier SQ=" ++ show (schemaQuality . classifier . brain $ wainAfterDecision)
  let deltaE = energyFor p a
  let wainRewarded = adjustEnergy deltaE "reward" wainAfterDecision
  putStrLn $ "Δe=" ++ show deltaE
  putStrLn $ "condition before=" ++ show (condition w) ++ " after=" ++ show (condition wainRewarded)
  putStrLn $ "happiness before=" ++ show (happiness w) ++ " after=" ++ show (happiness wainRewarded)
  -- let bmu = head . bmus . wdrClassifierReport $ report
  putStr $ "Choosing to " ++ show a ++ " in response to " ++ show p
  if deltaE < 0
    then putStrLn " was wrong"
    else putStrLn " was correct"
  let wainAfterReflection = reflect wainRewarded
  -- mapM_ putStrLn $ prettyReflectionReport wainAfterReflection rReflect
  -- keep the wain's energy constant
  let restorationEnergy = UI.wide (energy w) - UI.wide (energy wainAfterReflection)
  let wainFinal = adjustEnergy restorationEnergy "restoration" wainAfterReflection
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
  putStrLn "By about round 500, the predictor should have all the"
  putStrLn "correct actions."
  putStrLn "The predictor models will continue to vary, but the correct"
  putStrLn "actions should not change."
  putStrLn "After round 500, the only mistakes the wain should make are"
  putStrLn "near boundary values (50, 100, 150, 200)."
  let g = mkStdGen 263167 -- seed
  let ps = map TestPattern . take 1000 $ evalRand getRandoms g
  let nps = zip [1..] ps
  foldM_ tryOne testWain nps
  putStrLn "test complete"
