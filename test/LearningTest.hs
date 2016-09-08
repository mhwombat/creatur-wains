------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.LearningTest
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify that a wain can learn from experience.
--
------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ALife.Creatur.Wain
import ALife.Creatur.Wain.BrainInternal (classifier, predictor,
  makeBrain, decisionQuality, decisionReport)
import ALife.Creatur.Wain.ClassifierQC (TestTweaker(..))
import ALife.Creatur.Wain.GeneticSOMInternal (LearningParams(..),
  buildGeneticSOM, modelMap, schemaQuality)
import ALife.Creatur.Wain.PlusMinusOne (doubleToPM1)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Response (action, outcomes)
import ALife.Creatur.Wain.ResponseQC (TestAction(..), testActionDiff2)
import ALife.Creatur.Wain.SimpleResponseTweaker (ResponseTweaker(..))
import ALife.Creatur.Wain.Statistics (stats)
import ALife.Creatur.Wain.TestUtils (TestPattern(..))
import ALife.Creatur.Wain.UnitInterval (uiToDouble)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Lens
import Control.Monad (foldM_)
import Control.Monad.Random (mkStdGen, evalRand, getRandoms)
import qualified Data.Map.Strict as M
import Data.List (minimumBy)
import Data.Ord (comparing)

nTests :: Int
nTests = 1000

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
energyFor p a
  = if a == correctAnswer p then reward else -reward

testWain :: Wain TestPattern TestTweaker (ResponseTweaker TestAction) TestAction
testWain = w'
  where wName = "Fred"
        wAppearance = TestPattern 0
        wRw = makeWeights [0.2, 0.8]
        (Right wBrain) = makeBrain wClassifier wPredictor
                           wHappinessWeights 1 wRw 0.085
                           [doubleToPM1 reward, 0, 0, 0]
                           wIos wRds
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifierSize = 10
        wClassifier = buildGeneticSOM ec wClassifierSize 0.145 TestTweaker
        wIos = [doubleToPM1 reward, 0, 0, 0]
        wRds = [doubleToPM1 reward, 0, 0, 0]
        wPredictor = buildGeneticSOM ep (wClassifierSize*5) 0.1 ResponseTweaker
        wHappinessWeights = makeWeights [1, 0, 0, 0]
        ec = LearningParams 0.5 0.000000001 (fromIntegral nTests)
        ep = LearningParams 0.1 0.000000001 (fromIntegral nTests)
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

tryOne
  :: Wain TestPattern TestTweaker (ResponseTweaker TestAction) TestAction -> TestPattern
    -> IO (Wain TestPattern TestTweaker (ResponseTweaker TestAction) TestAction)
tryOne w p = do
  putStrLn $ "-----"
  putStrLn $ "stats=" ++ show (stats w)
  putStrLn "Initial classifier models"
  describeClassifierModels w
  putStrLn "Initial decision models"
  describePredictorModels w
  putStrLn $ "Wain sees " ++ show p
  let (ldss, xss, r, wainAfterDecision) = chooseAction testActionDiff2 [p] w
  putStrLn $ "ldss=" ++ show ldss
  let (cBMU, cDiff) = minimumBy (comparing snd) . head $ ldss
  mapM_ putStrLn $ concatMap decisionReport xss
  putStrLn $ "DEBUG classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ wainAfterDecision)
  -- describeClassifierModels wainAfterDecision
  -- describePredictorModels wainAfterDecision
  let a = view action r
  let deltaE = energyFor p a
  putStrLn $ "Wain sees " ++ show p ++ ", classifies it as "
    ++ show cBMU ++ " and chooses to " ++ show a
    ++ " predicting the outcomes " ++ show (view outcomes r)
  let (wainRewarded, _) = adjustEnergy deltaE wainAfterDecision
  putStrLn $ "Δe=" ++ show deltaE
  putStrLn $ "condition before=" ++ show (condition w) ++ " after=" ++ show (condition wainRewarded)
  putStrLn $ "happiness before=" ++ show (happiness w) ++ " after=" ++ show (happiness wainRewarded)
  putStr $ "Choosing to " ++ show a ++ " in response to " ++ show p
  if deltaE < 0
    then putStrLn " was wrong"
    else putStrLn " was correct"
  let (wainAfterReflection, err) = reflect [p] r w wainRewarded
  putStrLn $ "err=" ++ show err
  -- keep the wain's energy constant
  let restorationEnergy = uiToDouble (view energy w) - uiToDouble (view energy wainRewarded)
  let (wainFinal, _) = adjustEnergy restorationEnergy wainAfterReflection
  putStrLn "Final classifier models"
  describeClassifierModels wainFinal
  putStrLn "Final decision models"
  describePredictorModels wainFinal
  putStrLn $ "classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ w)
  putStrLn $ "predictor SQ=" ++ show (schemaQuality . view (brain . predictor) $ w)
  putStrLn $ "DQ=" ++ show (decisionQuality . view brain $ w)
  return wainFinal

describeClassifierModels :: Wain TestPattern TestTweaker (ResponseTweaker TestAction) TestAction -> IO ()
describeClassifierModels w = mapM_ (putStrLn . f) ms
  where ms = M.toList . modelMap . view (brain . classifier) $ w
        f (l, x) = view name w ++ "'s classifier model " ++ show l
                     ++ ": " ++ show x
                     ++ " (" ++ show (correctAnswer x) ++ ")"

describePredictorModels :: Wain TestPattern TestTweaker (ResponseTweaker TestAction) TestAction -> IO ()
describePredictorModels w = mapM_ (putStrLn . f) ms
  where ms = M.toList . modelMap . view (brain . predictor) $ w
        f (l, r) = view name w ++ "'s predictor model " ++ show l ++ ": "
                     ++ pretty r

main :: IO ()
main = do
  putStrLn "By about round 100, the wain should have 5 classifier"
  putStrLn "models that are roughly 25, 75, 125, 175 and 225."
  putStrLn "The classifier models should change very little after that."
  putStrLn "By about round 500, the predictor should have all the"
  putStrLn "correct actions."
  putStrLn "The predictor models will continue to vary, but the correct"
  putStrLn "actions should not change."
  putStrLn "After round 500, the only mistakes the wain should make are"
  putStrLn "near boundary values (50, 100, 150, 200)."
  let g = mkStdGen 263167 -- seed
  let ps = map TestPattern . take nTests $ evalRand getRandoms g
  foldM_ tryOne testWain ps
  putStrLn "test complete"
