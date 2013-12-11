------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.GeneratePopulation
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------

import ALife.Creatur (agentId)
import ALife.Creatur.Wain.Agent (Creatur.Wain, randomAgent)
import qualified ALife.Creatur.Wain.Config as Config
import ALife.Creatur.Logger (writeToLog)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Universe (SimpleUniverse, addAgent)
import ALife.Creatur.Wain.Statistics (stats, maxStats, minStats, avgStats)
import Control.Monad.Random (evalRandIO, Rand, RandomGen)
import Control.Monad.State.Lazy (StateT, evalStateT)

-- buildAgents :: DiploidReader [Wain]
-- buildAgents = do  
--   agents <- mapM (buildAgent True) names
--   return . catEithers $ agents

buildAgents :: RandomGen r => Rand r [Wain]
buildAgents = mapM (randomAgent Config.maxInitialSOMSize
                    Config.maxInitialLearningTime Config.imageWidth
                    Config.imageHeight) names

names :: [String]
names = map (("Founder" ++) . show) [1..Config.initialPopulationSize]

introduce :: Wain -> StateT (SimpleUniverse Wain) IO ()
introduce agent = do
  writeToLog $ "GeneratePopulation: Created " ++ agentId agent
  addAgent agent

introduceAll :: [Wain] -> StateT (SimpleUniverse Wain) IO ()
introduceAll agents = do
  mapM_ introduce agents
  let xs = map stats agents
  writeToLog $ pretty (maxStats xs)
  writeToLog $ pretty (minStats xs)
  writeToLog $ pretty (avgStats xs)
  
main :: IO ()
main = do
  -- r1 <- getStdGen -- source of random genes
  -- r2 <- getStdGen -- source of random genes

  -- let g1 = randoms r1
  -- let g2 = randoms r2

  -- let agents = runDiploidReader buildAgents (g1, g2)

  print names
  agents <- evalRandIO buildAgents
  evalStateT (introduceAll agents) Config.universe
