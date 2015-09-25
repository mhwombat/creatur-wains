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
import ALife.Creatur.Wain.Statistics (stats)
import ALife.Creatur.Wain.Response (action, outcomes)
import ALife.Creatur.Wain.ResponseQC (TestAction(..))
import ALife.Creatur.Wain.TestUtils (TestPattern(..))
import ALife.Creatur.Wain.GeneticSOMInternal (ExponentialParams(..),
  buildGeneticSOM, modelMap, schemaQuality)
import ALife.Creatur.Wain.BrainInternal (classifier, predictor,
  makeBrain, scenarioReport, responseReport, decisionReport,
  decisionQuality)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.UnitInterval (uiToDouble)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Lens
import Control.Monad (foldM_)
import Control.Monad.Random (mkStdGen, evalRand, getRandoms)
import qualified Data.Map.Strict as M
import Data.List (minimumBy)
import Data.Ord (comparing)

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
        wBrain = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifier = buildGeneticSOM ec 10 0.02 TestTweaker
        wMuser = makeMuser [0, 0, 0, 0] 2
        wPredictor = buildGeneticSOM ep 50 0.1 PredictorTweaker
        wHappinessWeights = makeWeights [1, 0, 0]
        -- This wain will be taught the correct actions up front.
        -- After storing those initial action models, it doesn't need to
        -- learn anything.
        ec = ExponentialParams 0.01 0.01
        ep = ExponentialParams 0.01 0.01
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

tryOne
  :: Wain TestPattern TestTweaker TestAction -> TestPattern
    -> IO (Wain TestPattern TestTweaker TestAction)
tryOne w p = do
  putStrLn $ "-----"
  putStrLn $ "stats=" ++ show (stats w)
  putStrLn "Initial classifier models"
  describeClassifierModels w
  putStrLn "Initial decision models"
  describePredictorModels w
  let (lds, sps, rplos, aos, r, wainAfterDecision) = chooseAction [p] w
  let (cBMU, cDiff) = minimumBy (comparing snd) . head $ lds
  mapM_ putStrLn $ scenarioReport sps
  mapM_ putStrLn $ responseReport rplos
  mapM_ putStrLn $ decisionReport aos
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
  foldM_ tryOne testWain ps
  putStrLn "test complete"
