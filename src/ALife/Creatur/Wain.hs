------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleContexts #-}
module ALife.Creatur.Wain
  (
    Wain(..),
    buildWain,
    adjustEnergy,
    coolPassion,
    identity,
    appearanceOf,
    mate,
    chooseAction,
    classify,
    learn,
    feedback,
    numberOfClassifierModels,
    numberOfDeciderModels,
    randomWain
  ) where

import ALife.Creatur (Agent, agentId, isAlive)
import ALife.Creatur.AgentNamer (genName)
import ALife.Creatur.Database (Record, key)
import ALife.Creatur.Genetics.BRGCWord8 (Genetic, DiploidReader,
  Sequence, get, put, copy, copy2, consumed2, getAndExpress,
  runDiploidReader, write)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Genetics.Recombination (mutatePairedLists,
  randomCrossover, randomCutAndSplice, randomOneOfPair,
  repeatWithProbability, withProbability)
import ALife.Creatur.Genetics.Reproduction.Sexual (Reproductive, Base,
  produceGamete, build, makeOffspring)
import ALife.Creatur.Universe (SimpleUniverse)
import qualified ALife.Creatur.Wain.Condition as C
import qualified ALife.Creatur.Wain.Brain as B
import ALife.Creatur.Wain.GeneticSOM (Label)
import ALife.Creatur.Wain.Random (RandomInitial, randomInitial)
import qualified ALife.Creatur.Wain.Scenario as S
import ALife.Creatur.Wain.Statistics (Statistical, stats, iStat)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, evalRandIO)
import Control.Monad.State.Lazy (StateT)
import Data.Datamining.Pattern (Pattern(..), Metric)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)
import System.Random (Random)

data Wain s a = Wain
  {
    name :: String,
    appearance :: s,
    brain :: B.Brain s a,
    condition :: C.Condition,
    genome :: ([Word8],[Word8])
  } deriving (Eq, Generic)

instance (Pattern p, Show p, Show (Metric p), Show a, Eq a)
      => Show (Wain p a) where
  show (Wain n a b c g) = "Wain " ++ n ++ " (" ++ show a ++ ") ("
    ++ show b ++ ") (" ++ show c ++ ") (" ++ show g ++ ")"

instance (Pattern s, Metric s ~ UIDouble) => Statistical (Wain s a) where
  stats (Wain _ _ b c (g1, g2)) =
    stats b ++ stats c
      ++ [iStat "genome length" (length g1 + length g2)]
                               
instance (Serialize s, Serialize a, Pattern s, Metric s ~ UIDouble, Eq a)
  => Serialize (Wain s a)

-- This implementation is useful for generating the genes in the
-- initial population, and for testing
instance (Genetic s, Genetic a, Pattern s, Metric s ~ UIDouble, Eq a)
      => Genetic (Wain s a) where
  put (Wain _ a b _ _) = put a >> put b
  get = do
    g <- copy
    let g2 = (g, g) :: ([Word8],[Word8])
    a <- get
    b <- get
    return $ Wain "" <$> a <*> b <*> pure C.initialCondition <*> pure g2

-- This implementation is useful for testing
instance (Diploid s, Diploid a, Pattern s, Metric s ~ UIDouble, Eq a)
      => Diploid (Wain s a) where
  express x y = Wain "" a b c ([],[])
    where a = express (appearance x) (appearance y)
          b = express (brain x) (brain y)
          c = C.initialCondition
          
instance Agent (Wain s a) where
  agentId = name
  isAlive = C.alive . condition

instance Record (Wain s a) where key = agentId

instance (Genetic s, Genetic a, Diploid s, Diploid a, Eq a, Pattern s,
         Metric s ~ UIDouble)
         => Reproductive (Wain s a) where
  type Base (Wain s a) = Sequence
  produceGamete a =
    repeatWithProbability 0.1 randomCrossover (genome a) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build n = runDiploidReader (buildWain False n)

buildWain
  :: (Genetic s, Genetic a, Diploid s, Diploid a, Eq a, Pattern s,
    Metric s ~ UIDouble)
    => Bool -> String -> DiploidReader (Either [String] (Wain s a))
buildWain truncateGenome n = do
  a <- getAndExpress
  b <- getAndExpress
  g <- if truncateGenome then consumed2 else copy2
  return $ Wain n <$> a <*> b <*> pure C.initialCondition <*> pure g

randomWain
  :: (RandomGen g, Pattern p, Metric p ~ UIDouble, Eq a, Random a, Genetic p, Genetic a)
    => String -> p -> Word8 -> [p] -> Word8 -> Rand g (Wain p a)
randomWain n app maxClassifierSize ps maxDeciderSize = do
  b <- B.randomBrain maxClassifierSize ps maxDeciderSize
  c <- randomInitial
  let strawMan = Wain n app b c ([], [])
  let g = write strawMan
  return $ Wain n app b c (g, g)

adjustEnergy
  :: UIDouble -> Wain s a -> Wain s a
adjustEnergy delta a = a {condition = C.adjustEnergy delta (condition a)}

coolPassion :: Wain s a -> Wain s a
coolPassion a = a {condition = C.coolPassion (condition a)}

data Object s a = DObject s String | AObject (Wain s a)

identity :: Object s a -> String
identity (DObject _ s) = "Image " ++ s
identity (AObject a) = agentId a

appearanceOf :: Object s a -> s
appearanceOf (DObject img _) = img
appearanceOf (AObject a) = appearance a

mate
  :: (Genetic s, Genetic a, Diploid s, Diploid a, Eq a, Pattern s,
    Metric s ~ UIDouble)
    => Wain s a -> Wain s a -> UIDouble -> StateT (SimpleUniverse (Wain s a)) IO [Wain s a]
mate me other matingEnergyDelta = do
      babyName <- genName
      (Right baby) <- liftIO $ evalRandIO (makeOffspring me other babyName)
      return [coolPassion . adjustEnergy matingEnergyDelta $ me,
              coolPassion . adjustEnergy matingEnergyDelta $ other,
              baby]

chooseAction :: (Pattern p, Metric p ~ UIDouble, Eq a, Enum a, Bounded a)
    => p -> p -> Wain p a -> (Label, a, Wain p a)
chooseAction p1 p2 w = (l, a, w {brain=b3})
  where (l, s1, b1) = B.classify p1 (brain w)
        (_, s2, b2) = B.classify p2 b1
        scenario = S.Scenario s1 s2 (condition w)
        (a, b3) = B.recommendAction scenario b2

classify :: (Pattern p, Metric p ~ UIDouble, Eq a, Enum a, Bounded a)
    => p -> Wain p a -> (Label, Wain p a)
classify p w = (s, w {brain=b})
  where (s, _, b) = B.classify p (brain w)

learn :: Pattern p => Wain p a -> p -> Label -> Wain p a
learn w p l = w { brain=B.learn (brain w) p l }

feedback
  :: (Pattern p, Metric p ~ UIDouble, Eq a)
    => UIDouble -> Wain p a -> Wain p a
feedback deltaHappiness w
  = w { brain=B.feedback deltaHappiness (brain w) }

numberOfClassifierModels
  :: (Pattern p, Metric p ~ UIDouble)
    => Wain p a -> Int
numberOfClassifierModels = B.numberOfClassifierModels . brain

numberOfDeciderModels
  :: (Pattern p, Metric p ~ UIDouble, Eq a)
    => Wain p a -> Int
numberOfDeciderModels = B.numberOfDeciderModels . brain
