module Main where

import Data.Time.Clock

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Util (fromEither)
import ALife.Creatur.Wain hiding (size)
import ALife.Creatur.Wain.Brain
import ALife.Creatur.Wain.ClassifierQC (TestThinker(..))
import ALife.Creatur.Wain.Condition
import ALife.Creatur.Wain.Decider
import ALife.Creatur.Wain.DeciderQC ()
import ALife.Creatur.Wain.GeneticSOM hiding (size)
import ALife.Creatur.Wain.Response
import ALife.Creatur.Wain.ResponseQC
import ALife.Creatur.Wain.Scenario
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.Util (unitInterval)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Lens
import Control.Monad (replicateM)
import Control.Monad.Random (Rand, Random, RandomGen, evalRandIO,
  getRandomR)
import qualified Data.Datamining.Clustering.Classifier as C
import Data.Datamining.Clustering.SSOM
import Data.Version (showVersion)
import Data.Word (Word8, Word16)
import Paths_creatur_wains (version)
import System.IO.Temp (withSystemTempDirectory)

makeDecider :: Int -> Int -> Decider TestAction
makeDecider n patternLength
  = buildGeneticSOM f t (replicate n modelResponse)
  where modelResponse = Response modelScenario Walk (Just 0)
        modelScenario = Scenario [xs, xs] modelCondition
        modelCondition = Condition 1 0 0
        xs = replicate patternLength 0
        f = ExponentialParams 1 1
        t = DeciderThinker (makeWeights [1,1,1])
              (makeWeights [1,1,1]) (makeWeights [1,1])

deciderBenchmark :: Decider TestAction -> IO ()
deciderBenchmark d = do
  putStrLn $ "model count=" ++ show (numModels d)
  putStrLn $ "object length="
    ++ show (length . head . view diffs . _scenario . head . C.models . view patternMap $ d)
  let x = W8.write d
  let d' = fromEither (error "read returned Nothing") . W8.read $ x
  putStrLn $ "passed=" ++ show (d' == d)

randomWain
  :: (RandomGen g)
    => String -> Int -> Int -> Word16 -> Rand g (Wain TestPattern TestThinker TestAction)
randomWain n classifierSize deciderSize maxAgeOfMaturity = do
  (app:ps) <- sequence . replicate classifierSize $ randomTestPattern
  fc <- randomExponential randomExponentialParams
  let c = buildGeneticSOM fc TestThinker ps
  fd <- randomExponential randomExponentialParams
  xs <- sequence . replicate deciderSize $ randomResponse 2 (numModels c) (-1, 1)
  let hw = makeWeights [1,1,1]
  let t = DeciderThinker (makeWeights [1,1,1])
              (makeWeights [1,1,1]) (makeWeights [1,1])
  let d = buildGeneticSOM fd t xs
  let b = Brain c d hw
  d <- getRandomR unitInterval
  m <- getRandomR (0,maxAgeOfMaturity)
  p <- getRandomR unitInterval
  return $ buildWainAndGenerateGenome n app b d m p

wainBenchmark :: Int -> Int -> FilePath -> IO ()
wainBenchmark classifierSize deciderSize dir = do
  w <- evalRandIO
        (randomWain "fred" classifierSize deciderSize 100)
          :: IO (Wain TestPattern TestThinker TestAction)
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
  runBenchmark "deciderBenchmark" (deciderBenchmark $ makeDecider 5 100)
  runBenchmark "wainBenchmark"
    (withSystemTempDirectory "creatur-wains" $ wainBenchmark 3 3)

