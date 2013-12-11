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
    Sex(..),
    buildWain,
    adjustEnergy,
    coolPassion,
    identity,
    appearanceOf,
    decide,
    allowMating,
    chooseAction,
    feedback,
    numberOfClassifierModels,
    numberOfDeciderModels,
    randomWain
  ) where

--import ALife.Realta.Brain (Brain)
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
--import ALife.Creatur.Logger (writeToLog)
import qualified ALife.Creatur.Wain.Condition as C
import qualified ALife.Creatur.Wain.Brain as B
import ALife.Creatur.Wain.Random (RandomInitial, randomInitial)
import qualified ALife.Creatur.Wain.Scenario as S
import ALife.Creatur.Wain.Statistics (Statistical, stats, iStat)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, fromList, evalRandIO)
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
    sex :: Sex,
    condition :: C.Condition,
    genome :: ([Word8],[Word8])
  } deriving (Eq, Generic)

instance (Pattern p, Show p, Show (Metric p), Show a, Eq a)
      => Show (Wain p a) where
  show (Wain n a b s c g) = "Wain " ++ n ++ " (" ++ show a ++ ") ("
    ++ show b ++ ") (" ++ show s ++ ") (" ++ show c ++ ") ("
    ++ show g ++ ")"

instance (Pattern s, Metric s ~ UIDouble) => Statistical (Wain s a) where
  stats (Wain _ _ b _ c (g1, g2)) =
    stats b ++ stats c
      ++ [iStat "genome length" (length g1 + length g2)]
                               
instance (Serialize s, Serialize a, Pattern s, Metric s ~ UIDouble, Eq a)
  => Serialize (Wain s a)

-- This implementation is useful for generating the genes in the
-- initial population, and for testing
instance (Genetic s, Genetic a, Pattern s, Metric s ~ UIDouble, Eq a)
      => Genetic (Wain s a) where
  put (Wain _ a b s _ _) = put a >> put b >> put s
  get = do
    g <- copy
    let g2 = (g, g) :: ([Word8],[Word8])
    a <- get
    b <- get
    s <- get
    return $ Wain "" <$> a <*> b <*> s
      <*> pure C.initialCondition <*> pure g2

-- This implementation is useful for testing
instance (Diploid s, Diploid a, Pattern s, Metric s ~ UIDouble, Eq a)
      => Diploid (Wain s a) where
  express x y = Wain "" a b s c ([],[])
    where a = express (appearance x) (appearance y)
          b = express (brain x) (brain y)
          s = express (sex x) (sex y)
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

data Sex = Male | Female deriving (Show, Eq, Generic)
instance Serialize Sex
instance Genetic Sex
instance Diploid Sex

instance RandomInitial Sex where
  randomInitial = fromList [(Male, 0.5), (Female, 0.5)]

buildWain
  :: (Genetic s, Genetic a, Diploid s, Diploid a, Eq a, Pattern s,
    Metric s ~ UIDouble)
    => Bool -> String -> DiploidReader (Either [String] (Wain s a))
buildWain truncateGenome n = do
  a <- getAndExpress
  b <- getAndExpress
  s <- getAndExpress
  g <- if truncateGenome then consumed2 else copy2
  return $ Wain n <$> a <*> b <*> s
    <*> pure C.initialCondition <*> pure g

randomWain
  :: (RandomGen g, Pattern p, Metric p ~ UIDouble, Eq a, Random a, Genetic p, Genetic a)
    => String -> Word8 -> [p] -> Word8 -> Rand g (Wain p a)
randomWain n maxClassifierSize patternSource maxDeciderSize = do
  let (app:ps) = patternSource
  b <- B.randomBrain maxClassifierSize ps maxDeciderSize
  s <- randomInitial
  c <- randomInitial
  let strawMan = Wain n app b s c ([], [])
  let g = write strawMan
  return $ Wain n app b s c (g, g)

-- withBrain :: Monad m => StateT (B.Brain s a) m a -> StateT (Wain s a) m a
-- withBrain f = do
--   a <- S.get
--   stateMap (\b -> a {brain=b}) brain f

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

decide
  :: (Eq a, Enum a, Bounded a, Pattern s, Metric s ~ UIDouble)
    => [UIDouble] -> [UIDouble] -> Wain s a -> StateT (SimpleUniverse a) IO (Wain s a, a)
decide x y me@(Wain _ _ b _ c _) = do
  --  writeToLog $ "DEBUG: " ++ name ++ " is thinking about what to do"
  --  writeToLog $ "DEBUG: brain=" ++ show brain
  let scenario = S.Scenario x y c
  let (action, b') = B.recommendAction scenario b
  --  writeToLog $ "DEBUG: " ++ name ++ " made a decision"
  --  writeToLog $ "DEBUG: brain'=" ++ show brain'
  --  writeToLog $ "DEBUG: action=" ++ show action
  return (me { brain=b' }, action)

allowMating
  :: (Genetic s, Genetic a, Diploid s, Diploid a, Eq a, Pattern s,
    Metric s ~ UIDouble)
    => [Wain s a] -> UIDouble -> StateT (SimpleUniverse (Wain s a)) IO [Wain s a]
allowMating (me:other:_) matingEnergyDelta = do
  if sex me == Female && sex other == Male
    then do
      babyName <- genName
      (Right baby) <- liftIO $ evalRandIO (makeOffspring me other babyName)
      -- writeToLog $ 
      --   name me ++ " and " ++ name other ++
      --     " gave birth to " ++ name ++ ", a " ++ 
      --     show (sex baby) ++ " wain"
      -- writeToLog $ "Mother: " ++ show me
      -- writeToLog $ "Father: " ++ show other
      -- writeToLog $ "Baby: " ++ show baby
      return [coolPassion . adjustEnergy matingEnergyDelta $ me,
              coolPassion . adjustEnergy matingEnergyDelta $ other,
              baby]
    else return []
allowMating xs _ = return xs -- need two agents to mate

chooseAction :: (Pattern p, Metric p ~ UIDouble, Eq a, Enum a, Bounded a)
    => p -> p -> Wain p a -> (a, Wain p a)
chooseAction p1 p2 w = (a, w {brain=b3} )
  where (s1, b1) = B.classify p1 (brain w)
        (s2, b2) = B.classify p2 b1
        scenario = S.Scenario s1 s2 (condition w)
        (a, b3) = B.recommendAction scenario b2

feedback
  :: (Pattern p, Metric p ~ UIDouble, Eq a)
    => UIDouble -> Wain p a -> Wain p a
feedback deltaHappiness w
  = w { brain=B.feedback deltaHappiness (brain w) }

numberOfClassifierModels :: (Pattern p, Metric p ~ UIDouble) => Wain p a -> Int
numberOfClassifierModels = B.numberOfClassifierModels . brain

numberOfDeciderModels :: (Pattern p, Metric p ~ UIDouble, Eq a) => Wain p a -> Int
numberOfDeciderModels = B.numberOfDeciderModels . brain
