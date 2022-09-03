module Main where

import Data.Time.Clock

import ALife.Creatur.Genetics.BRGCWord8 qualified as W8
import ALife.Creatur.Util               (fromEither)
import ALife.Creatur.Wain               hiding (size)
import ALife.Creatur.Wain.Brain
import ALife.Creatur.Wain.ClassifierQC  (TestTweaker (..))
import ALife.Creatur.Wain.GeneticSOM    hiding (size)
import ALife.Creatur.Wain.Muser
import ALife.Creatur.Wain.Predictor
import ALife.Creatur.Wain.PredictorQC   ()
import ALife.Creatur.Wain.Response
import ALife.Creatur.Wain.ResponseQC
import ALife.Creatur.Wain.Scenario
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.UnitInterval  (UI.Double)
import ALife.Creatur.Wain.Util          (unitInterval)
import ALife.Creatur.Wain.Weights       (makeWeights)
import Control.Monad                    (replicateM)
import Control.Monad.Random             (Rand, Random, RandomGen, evalRandIO,
                                         getRandom, getRandomR)
import Data.Map.Strict                  qualified as M
import Data.Version                     (showVersion)
import Data.Word                        (Word16, Word8)
import Paths_creatur_wains              (version)
import System.IO.Temp                   (withSystemTempDirectory)

makePredictor :: Word16 -> UI.Double -> Predictor TestAction
makePredictor n dt = buildGeneticSOM e n dt t
  where e = LearningParams 1 1
        t = ResponseTweaker (makeWeights [1,1,1])
              (makeWeights [1,1,1])

predictorBenchmark :: Predictor TestAction -> IO ()
predictorBenchmark d = do
  putStrLn $ "model count=" ++ show (numModels d)
  putStrLn $ "object length="
    ++ show (length . labels . _scenario . head . M.elems
         . modelMap $ d)
  let x = W8.write d
  let d' = fromEither (error "read returned Nothing") . W8.read $ x
  putStrLn $ "passed=" ++ show (d' == d)

randomWain
  :: (RandomGen g)
    => String -> Word16 -> UI.Double -> Word16 -> UI.Double -> Word16
      -> Rand g (Wain TestPattern TestTweaker TestAction)
randomWain wName classifierSize classifierThreshold
    predictorSize predictorThreshold maxAgeOfMaturity = do
  wAppearance <- randomTestPattern
  fc <- randomExponential randomLearningParams
  let c = buildGeneticSOM fc classifierSize classifierThreshold TestTweaker
  fd <- randomExponential randomLearningParams
  let hw = makeWeights [1,1,1]
  let t = ResponseTweaker (makeWeights [1,1,1]) (makeWeights [1,1]) :: ResponseTweaker TestAction
  let d = buildGeneticSOM fd predictorSize predictorThreshold t
  let wBrain = makeBrain c (makeMuser 0 2) d hw
  wDevotion <- getRandomR unitInterval
  wAgeOfMaturity <- getRandomR (0,maxAgeOfMaturity)
  wPassionDelta <- getRandom
  return $ buildWainAndGenerateGenome wName wAppearance wBrain wDevotion
   wAgeOfMaturity wPassionDelta

wainBenchmark :: Word16 -> Word16 -> FilePath -> IO ()
wainBenchmark classifierSize predictorSize dir = do
  w <- evalRandIO
        (randomWain "fred" classifierSize 0.1 predictorSize 0.1 100)
          :: IO (Wain TestPattern TestTweaker TestAction)
  let filename = dir ++ "/pattern"
  writeFile filename $ show w

runBenchmark :: String -> IO () -> IO ()
runBenchmark s b = do
  start <- getCurrentTime
  b
  end <- getCurrentTime
  putStrLn $ s ++ " took " ++ show (diffUTCTime end start)

main :: IO ()
main = do
  putStrLn $ "creatur-wains v" ++ showVersion version
  runBenchmark "predictorBenchmark" (predictorBenchmark $ makePredictor 5 0.1)
  runBenchmark "wainBenchmark"
    (withSystemTempDirectory "creatur-wains" $ wainBenchmark 3 3)
