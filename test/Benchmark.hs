module Main where

import Data.Time.Clock

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Util
import ALife.Creatur.Wain hiding (size)
import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.Condition
import ALife.Creatur.Wain.Decider
import ALife.Creatur.Wain.DeciderQC ()
import ALife.Creatur.Wain.Response
import ALife.Creatur.Wain.ResponseQC
import ALife.Creatur.Wain.Scenario
import ALife.Creatur.Wain.TestUtils
import Control.Monad (replicateM)
import Control.Monad.Random (evalRandIO)
import Data.Datamining.Clustering.SOM
import Data.Version (showVersion)
import Data.Word (Word8)
import Math.Geometry.Grid
import Math.Geometry.GridMap
import Paths_creatur_wains (version)
import System.IO.Temp (withSystemTempDirectory)

makeDecider :: Int -> Int -> Decider TestAction
makeDecider gridSize patternLength
  = buildGeneticSOM (fromIntegral gridSize) f (repeat modelResponse)
  where modelResponse = Response modelScenario Smile (Just 0)
        modelScenario = Scenario xs xs modelCondition
        modelCondition = Condition 1 0
        xs = replicate patternLength 0
        f = DecayingGaussian 1 0 (fromIntegral gridSize) 0 100

deciderBenchmark :: Decider TestAction -> IO ()
deciderBenchmark d = do
  putStrLn $ "grid size=" ++ show (size . toGrid $ sSOM d)
  putStrLn $ "tile count=" ++ show (tileCount $ sSOM d)
  putStrLn $ "object length="
    ++ show (length . directObject . scenario . head $ models d)
  let x = W8.write d
  let d' = fromEither (error "read returned Nothing") . W8.read $ x
  putStrLn $ "passed=" ++ show (d' == d)

wainBenchmark :: Word8 -> Word8 -> FilePath -> IO ()
wainBenchmark classifierSize deciderSize dir = do
  let n = fromIntegral (3*classifierSize*classifierSize)
  (app:ps) <- evalRandIO (replicateM n $ randomTestPattern)
  w <- evalRandIO
        (randomWain "fred" app classifierSize ps deciderSize 100)
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
