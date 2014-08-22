module Main where

import Data.Time.Clock

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Util
import ALife.Creatur.Wain hiding (size)
import ALife.Creatur.Wain.Brain
import ALife.Creatur.Wain.Condition
import ALife.Creatur.Wain.Decider
import ALife.Creatur.Wain.DeciderQC ()
import ALife.Creatur.Wain.GeneticSOM hiding (size)
import ALife.Creatur.Wain.Response
import ALife.Creatur.Wain.ResponseQC
import ALife.Creatur.Wain.Scenario
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.Util (unitInterval)
import Control.Monad (replicateM)
import Control.Monad.Random (Rand, Random, RandomGen, evalRandIO,
  getRandomR)
import qualified Data.Datamining.Clustering.Classifier as C
import Data.Datamining.Clustering.SOM
import Data.Version (showVersion)
import Data.Word (Word8, Word16)
import Math.Geometry.Grid
import Math.Geometry.GridMap
import Paths_creatur_wains (version)
import System.IO.Temp (withSystemTempDirectory)

makeDecider :: Int -> Int -> Decider TestAction
makeDecider gridSize patternLength
  = buildGeneticSOM (fromIntegral gridSize) f (repeat modelResponse)
  where modelResponse = Response modelScenario Smile (Just 0)
        modelScenario = Scenario xs xs modelCondition
        modelCondition = Condition 1 0 0
        xs = replicate patternLength 0
        f = DecayingGaussian 1 0 (fromIntegral gridSize) 0 100

deciderBenchmark :: Decider TestAction -> IO ()
deciderBenchmark d = do
  putStrLn $ "grid size=" ++ show (size . toGrid $ patternMap d)
  putStrLn $ "tile count=" ++ show (tileCount $ patternMap d)
  putStrLn $ "object length="
    ++ show (length . directObject . scenario . head . C.models $ patternMap d)
  let x = W8.write d
  let d' = fromEither (error "read returned Nothing") . W8.read $ x
  putStrLn $ "passed=" ++ show (d' == d)

randomWain
  :: (RandomGen g)
    => String -> Word8 -> Word8 -> Word16 -> Rand g (Wain TestPattern TestAction)
randomWain n classifierSize deciderSize maxAgeOfMaturity = do
  (app:ps) <- sequence . repeat $ randomTestPattern
  fc <- randomDecayingGaussian
         (randomDecayingGaussianParams classifierSize)
  let c = buildGeneticSOM classifierSize fc ps
  fd <- randomDecayingGaussian
         (randomDecayingGaussianParams deciderSize)
  xs <- sequence . repeat $ randomResponse (numModels c) 
  let d = buildGeneticSOM deciderSize fd xs
  let b = buildBrain c d
  d <- getRandomR unitInterval
  m <- getRandomR (0,maxAgeOfMaturity)
  p <- getRandomR unitInterval
  return $ buildWainAndGenerateGenome n app b d m p

wainBenchmark :: Word8 -> Word8 -> FilePath -> IO ()
wainBenchmark classifierSize deciderSize dir = do
  w <- evalRandIO
        (randomWain "fred" classifierSize deciderSize 100)
          :: IO (Wain TestPattern TestAction)
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

