------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.LearningTest2
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify that a wain can learn by being given the correct answer
-- after the fact.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           ALife.Creatur.Wain.BrainInternal
    (classifier, decisionQuality, makeBrain, predictor)
import           ALife.Creatur.Wain.Classifier              (bmus)
import           ALife.Creatur.Wain.ClassifierQC
    (TestClassifierTweaker (..))
import           ALife.Creatur.Wain.GeneticSOMInternal
    (LearningParams (..), buildGeneticSOM, schemaQuality)
import           ALife.Creatur.Gene.Numeric.PlusMinusOne    (doubleToPM1)
import           ALife.Creatur.Wain.Response
    (action, labels)
import           ALife.Creatur.Wain.ResponseQC              (TestAction (..))
import           ALife.Creatur.Wain.SimpleMuser
    (SimpleMuser, makeMuser)
import           ALife.Creatur.Wain.SimpleResponseTweaker
    (ResponseTweaker (..))
import           ALife.Creatur.Wain.SimpleResponseTweakerQC ()
import           ALife.Creatur.Wain.Statistics              (stats)
import           ALife.Creatur.Gene.Test                    (TestPattern (..))
import           ALife.Creatur.Gene.Numeric.UnitInterval    (uiToDouble)
import           ALife.Creatur.Gene.Numeric.Weights         (makeWeights)
import           ALife.Creatur.WainInternal
import           Control.Lens
import           Control.Monad                              (foldM_)
import           Control.Monad.Random
    (evalRand, getRandoms, mkStdGen)

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

type TestWain = Wain TestPattern TestClassifierTweaker (ResponseTweaker TestAction) (SimpleMuser TestAction) TestAction

testWain :: TestWain
testWain = w'
  where wName = "Fred"
        wAppearance = TestPattern 0
        (Right wBrain) = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1 16 wIos wRds
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifierSize = 10
        wClassifier = buildGeneticSOM ec wClassifierSize TestClassifierTweaker
        (Right wMuser) = makeMuser [0, 0, 0, 0] 3
        wIos = [doubleToPM1 reward, 0, 0, 0]
        wRds = [doubleToPM1 reward, 0, 0, 0]
        wPredictor = buildGeneticSOM ep (wClassifierSize*5) ResponseTweaker
        wHappinessWeights = makeWeights [1, 0, 0, 0]
        ec = LearningParams 0.1 0.0001 1000
        ep = LearningParams 0.1 0.0001 1000
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

tryOne :: TestWain -> (Int, TestPattern) -> IO TestWain
tryOne w (n, p) = do
  putStrLn $ "----- Round " ++ show n ++ " -----"
  putStrLn $ "stats=" ++ show (stats w)
  putStrLn "Initial classifier models"
  mapM_ putStrLn $ prettyClassifierModels w
  putStrLn "Initial decision models"
  mapM_ putStrLn $ prettyBrainSummary w
  let (report, r, wainAfterDecision) = chooseAction [p] w
  mapM_ putStrLn $ prettyClassificationReport wainAfterDecision report
  mapM_ putStrLn $ prettyScenarioReport wainAfterDecision report
  mapM_ putStrLn $ prettyPredictionReport wainAfterDecision report
  mapM_ putStrLn $ prettyActionReport wainAfterDecision report
  putStrLn $ "DEBUG classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ wainAfterDecision)
  let a = view action r
  let deltaE = energyFor p a
  let (wainRewarded, _) = adjustEnergy deltaE wainAfterDecision
  putStrLn $ "Δe=" ++ show deltaE
  putStrLn $ "condition before=" ++ show (condition w) ++ " after=" ++ show (condition wainRewarded)
  putStrLn $ "happiness before=" ++ show (happiness w) ++ " after=" ++ show (happiness wainRewarded)
  let bmu = head . bmus . wdrClassifierReport $ report
  putStr $ "Choosing to " ++ show a ++ " in response to " ++ show p ++ " (BMU " ++ show bmu
  if deltaE < 0
    then putStrLn ") was wrong"
    else putStrLn ") was correct"
  let (rReflect, wainAfterReflection) = reflect r w wainRewarded
  mapM_ putStrLn $ prettyReflectionReport wainAfterReflection rReflect
  let ls = view labels r
  let (iReport, wAfterImprint) = imprintResponse ls (correctAnswer p) wainAfterReflection
  mapM_ putStrLn $ prettyResponseImprintReport wAfterImprint iReport
  -- keep the wain's energy constant
  let restorationEnergy = uiToDouble (view energy w) - uiToDouble (view energy wAfterImprint)
  let (wainFinal, _) = adjustEnergy restorationEnergy wAfterImprint
  putStrLn "Final classifier models"
  mapM_ putStrLn $ prettyClassifierModels wainFinal
  putStrLn "Final decision models"
  mapM_ putStrLn $ prettyBrainSummary wainFinal
  putStrLn $ "classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ w)
  putStrLn $ "predictor SQ=" ++ show (schemaQuality . view (brain . predictor) $ w)
  putStrLn $ "DQ=" ++ show (decisionQuality . view brain $ w)
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
