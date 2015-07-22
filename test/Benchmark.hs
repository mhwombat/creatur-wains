module Main where

import Data.Time.Clock

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Util (fromEither)
import ALife.Creatur.Wain hiding (size)
import ALife.Creatur.Wain.Brain
import ALife.Creatur.Wain.ClassifierQC (TestTweaker(..))
import ALife.Creatur.Wain.Decider
import ALife.Creatur.Wain.DeciderQC ()
import ALife.Creatur.Wain.GeneticSOM hiding (size)
import ALife.Creatur.Wain.Response
import ALife.Creatur.Wain.ResponseQC
import ALife.Creatur.Wain.Scenario
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.Util (unitInterval)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Lens
import Control.Monad (replicateM)
import Control.Monad.Random (Rand, Random, RandomGen, evalRandIO,
  getRandom, getRandomR)
import qualified Data.Map.Strict as M
import Data.Version (showVersion)
import Data.Word (Word8, Word16)
import Paths_creatur_wains (version)
import System.IO.Temp (withSystemTempDirectory)

makeDecider :: Word16 -> UIDouble -> Decider TestAction
makeDecider n dt = buildGeneticSOM e n dt t
  where e = ExponentialParams 1 1
        t = DeciderTweaker (makeWeights [1,1,1])
              (makeWeights [1,1,1]) (makeWeights [1,1])

deciderBenchmark :: Decider TestAction -> IO ()
deciderBenchmark d = do
  putStrLn $ "model count=" ++ show (numModels d)
  putStrLn $ "object length="
    ++ show (length . head . view diffs . _scenario . head . M.elems
         . modelMap $ d)
  let x = W8.write d
  let d' = fromEither (error "read returned Nothing") . W8.read $ x
  putStrLn $ "passed=" ++ show (d' == d)

randomWain
  :: (RandomGen g)
    => String -> Word16 -> UIDouble -> Word16 -> UIDouble -> Word16
      -> Rand g (Wain TestPattern TestTweaker TestAction)
randomWain wName classifierSize classifierThreshold
    deciderSize deciderThreshold maxAgeOfMaturity = do
  wAppearance <- randomTestPattern
  fc <- randomExponential randomExponentialParams
  let c = buildGeneticSOM fc classifierSize classifierThreshold TestTweaker
  fd <- randomExponential randomExponentialParams
  let hw = makeWeights [1,1,1]
  let t = DeciderTweaker (makeWeights [1,1,1])
              (makeWeights [1,1,1]) (makeWeights [1,1])
  let d = buildGeneticSOM fd deciderSize deciderThreshold t
  let wBrain = Brain c d hw
  wDevotion <- getRandomR unitInterval
  wAgeOfMaturity <- getRandomR (0,maxAgeOfMaturity)
  wPassionDelta <- getRandom
  wDefaultOutcome <- getRandom
  return $ buildWainAndGenerateGenome wName wAppearance wBrain wDevotion
    wAgeOfMaturity wPassionDelta wDefaultOutcome

wainBenchmark :: Word16 -> Word16 -> FilePath -> IO ()
wainBenchmark classifierSize deciderSize dir = do
  w <- evalRandIO
        (randomWain "fred" classifierSize 0.1 deciderSize 0.1 100)
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
  runBenchmark "deciderBenchmark" (deciderBenchmark $ makeDecider 5 0.1)
  runBenchmark "wainBenchmark"
    (withSystemTempDirectory "creatur-wains" $ wainBenchmark 3 3)

