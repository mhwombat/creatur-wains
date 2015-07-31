------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.TeachingTest
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify that a wain can be taught.
--
------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ALife.Creatur.Wain
import ALife.Creatur.Wain.ClassifierQC (TestTweaker(..))
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Predictor (PredictorTweaker(..))
import ALife.Creatur.Wain.Scenario (labels)
import ALife.Creatur.Wain.Response (action, scenario)
import ALife.Creatur.Wain.ResponseQC (TestAction(..))
import ALife.Creatur.Wain.TestUtils (TestPattern(..))
import ALife.Creatur.Wain.GeneticSOMInternal (ExponentialParams(..),
  buildGeneticSOM, modelMap)
import ALife.Creatur.Wain.BrainInternal (Brain(..), classifier,
  predictor, makeBrain)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.UnitInterval (uiToDouble)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Lens
import Control.Monad (foldM_)
import Control.Monad.Random (evalRandIO, getRandoms)
import qualified Data.Map.Strict as M

energyFor :: TestPattern -> TestAction -> Double
energyFor (TestPattern p) a
  | p < 50   = if a == Walk then 1 else -0.1
  | p < 100   = if a == Run then 1 else -0.1
  | p < 150   = if a == Jump then 1 else -0.1
  | p < 200   = if a == Skip then 1 else -0.1
  | otherwise = if a == Crawl then 1 else -0.1

imprintAll
  :: Wain TestPattern TestTweaker TestAction
    -> Wain TestPattern TestTweaker TestAction
imprintAll w = imprint [TestPattern 25] Walk
                 . imprint [TestPattern 75] Run
                 . imprint [TestPattern 125] Jump
                 . imprint [TestPattern 175] Skip
                 . imprint [TestPattern 225] Crawl $ w

testWain :: Wain TestPattern TestTweaker TestAction
testWain = imprintAll w'
  where wName = "Fred"
        wAppearance = TestPattern 0
        wBrain = makeBrain wClassifier wMuser wPredictor wHappinessWeights
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wClassifier = buildGeneticSOM ec 10 0.1 TestTweaker
        wMuser = makeMuser 0 2
        wPredictor = buildGeneticSOM ep 50 0.1 dtw
        wHappinessWeights = makeWeights [1, 0, 0]
        ec = ExponentialParams 0.2 0.1
        ep = ExponentialParams 0.1 0.01
        dtw = PredictorTweaker cw rw
        cw = makeWeights [1, 0, 0]
        rw = makeWeights [0.9, 0.1]
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta
        (w', _, _) = adjustEnergy 0.5 w

tryOne
  :: Wain TestPattern TestTweaker TestAction -> TestPattern
    -> IO (Wain TestPattern TestTweaker TestAction)
tryOne w p = do
  putStrLn $ "-----"
  putStrLn $ "Wain sees " ++ show p
  let (cBMUs, _, pBMU, _, r, wainAfterDecision)
        = chooseAction [p] w
  -- describeClassifierModels wainAfterDecision
  -- describePredictorModels wainAfterDecision
  let a = view action r
  let deltaE = energyFor p a
  putStrLn
    $ "Wain classifies it as " ++ show (head cBMUs)
      ++ " and chooses to " ++ show a
      ++ " based on predictor model " ++ show pBMU
  let (wainRewarded, _, _) = adjustEnergy deltaE wainAfterDecision
  putStrLn $ "Δe=" ++ show deltaE
  putStrLn $ "condition before=" ++ show (condition w) ++ " after=" ++ show (condition wainRewarded)
  putStrLn $ "happiness before=" ++ show (happiness w) ++ " after=" ++ show (happiness wainRewarded)
  if deltaE < 0
    then putStrLn "That was wrong"
    else putStrLn "That was correct"
  let (wainAfterReflection, err) = reflect [p] r wainRewarded
  putStrLn $ "err=" ++ show err
  -- keep the wain's energy constant
  let restorationEnergy = uiToDouble (view energy w) - uiToDouble (view energy wainRewarded)
  let (wainFinal, _, _) = adjustEnergy restorationEnergy wainAfterReflection
  putStrLn "Final classifier models"
  describeClassifierModels wainFinal
  putStrLn "Final decision models"
  describePredictorModels wainFinal
  return wainFinal

describeClassifierModels :: Wain TestPattern TestTweaker TestAction -> IO ()
describeClassifierModels w = mapM_ (putStrLn . f) ms
  where ms = M.toList . modelMap . view (brain . classifier) $ w
        f (l, r) = view name w ++ "'s classifier model " ++ show l ++ ": "
                     ++ show r

describePredictorModels :: Wain TestPattern TestTweaker TestAction -> IO ()
describePredictorModels w = mapM_ (putStrLn . f) ms
  where ms = M.toList . modelMap . view (brain . predictor) $ w
        f (l, r) = view name w ++ "'s predictor model " ++ show l ++ ": "
                     ++ pretty r

main :: IO ()
main = do
  ps <- map TestPattern . take 1000 <$> evalRandIO getRandoms
  foldM_ tryOne testWain ps
  putStrLn "test complete"
