module Main where

import Data.Time.Clock

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Util
import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.Condition
import ALife.Creatur.Wain.Decider
import ALife.Creatur.Wain.DeciderQC ()
import ALife.Creatur.Wain.Response
import ALife.Creatur.Wain.ResponseQC
import ALife.Creatur.Wain.Scenario
import Data.Datamining.Clustering.SOM
import Data.Version (showVersion)
import Math.Geometry.Grid
import Math.Geometry.GridMap
import Paths_creatur_wains (version)

makeDecider :: Int -> Int -> Decider TestAction
makeDecider gridSize patternLength
  = buildGeneticSOM (fromIntegral gridSize) f (repeat modelResponse)
  where modelResponse = Response modelScenario Smile (Just 0)
        modelScenario = Scenario xs xs modelCondition
        modelCondition = Condition 1 0
        xs = replicate patternLength 0
        f = DecayingGaussian 1 0 (fromIntegral gridSize) 0 100

runDeciderBenchmark :: Decider TestAction -> IO ()
runDeciderBenchmark d = do
  putStrLn $ "grid size=" ++ show (size . toGrid $ sSOM d)
  putStrLn $ "tile count=" ++ show (tileCount $ sSOM d)
  putStrLn $ "object length=" ++ show (length . directObject . scenario . head $ models d)
  let x = W8.write d
  let d' = fromEither (error "read returned Nothing") . W8.read $ x
  putStrLn $ "passed=" ++ show (d' == d)

main :: IO ()
main = do
  let v = "creatur-wains v" ++ showVersion version
  start <- getCurrentTime
  runDeciderBenchmark $ makeDecider 5 100
  end <- getCurrentTime
  putStrLn $ "decider benchmark " ++ v ++ " took " ++ show (diffUTCTime end start)
