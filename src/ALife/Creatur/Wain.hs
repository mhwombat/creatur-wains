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
{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleContexts,
    StandaloneDeriving #-}
module ALife.Creatur.Wain
  (
    Wain(..),
    Label,
    buildWain,
    adjustEnergy,
    coolPassion,
    identity,
    appearanceOf,
    hasChild,
    tryMating,
    weanChildIfReady,
    chooseAction,
    classify,
    teachLabel,
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
import ALife.Creatur.Logger (writeToLog)
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
import Control.Monad.Random (Rand, RandomGen, evalRandIO, getRandomR)
import Control.Monad.State.Lazy (StateT)
import Data.Datamining.Pattern (Pattern(..), Metric)
import Data.Serialize (Serialize)
import Data.Maybe (isJust)
import Data.Word (Word8, Word16)
import GHC.Generics (Generic)
import System.Random (Random)

data Wain s a = Wain
  {
    name :: String,
    appearance :: s,
    brain :: B.Brain s a,
    ageOfMaturity :: Word16,
    condition :: C.Condition,
    age :: Word16,
    numberOfChildren :: Word16,
    child :: Maybe (Wain s a),
    genome :: ([Word8],[Word8])
  } deriving (Eq, Generic)

deriving instance (Pattern p, Show p, Show (Metric p), Show a, Eq a)
      => Show (Wain p a)
 -- where
 --  show (Wain n a b c g) = "Wain " ++ n ++ " (" ++ show a ++ ") ("
 --    ++ show b ++ ") (" ++ show c ++ ") (" ++ show g ++ ")"

instance (Pattern s, Metric s ~ UIDouble) => Statistical (Wain s a) where
  stats w =
    iStat "age" (fromIntegral . age $ w)
      : iStat "total # of children" (fromIntegral . numberOfChildren $ w)
      : iStat "current # of children" (if hasChild w then 1 else 0)
      : stats (brain w)
      ++ stats (condition w)
      ++ [iStat "genome length" ( (length . fst . genome $ w)
                                  + (length . snd . genome $ w) )]
                               
instance (Serialize s, Serialize a, Pattern s, Metric s ~ UIDouble, Eq a)
  => Serialize (Wain s a)

-- This implementation is useful for generating the genes in the
-- initial population, and for testing
instance (Genetic s, Genetic a, Pattern s, Metric s ~ UIDouble, Eq a)
      => Genetic (Wain s a) where
  put w = put (appearance w) >> put (brain w) >> put (ageOfMaturity w)
  get = do
    g <- copy
    let g2 = (g, g) :: ([Word8],[Word8])
    a <- get
    b <- get
    m <- get
    return $ Wain "" <$> a <*> b <*> m <*> pure C.initialCondition
      <*> pure 0 <*> pure 0 <*> pure Nothing <*> pure g2

-- This implementation is useful for testing
instance (Diploid s, Diploid a, Pattern s, Metric s ~ UIDouble, Eq a)
      => Diploid (Wain s a) where
  express x y = Wain "" a b m c 0 0 Nothing ([],[])
    where a = express (appearance x) (appearance y)
          b = express (brain x) (brain y)
          m = express (ageOfMaturity x) (ageOfMaturity y)
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

hasChild :: Wain s a -> Bool
hasChild = isJust . child

buildWain
  :: (Genetic s, Genetic a, Diploid s, Diploid a, Eq a, Pattern s,
    Metric s ~ UIDouble)
    => Bool -> String -> DiploidReader (Either [String] (Wain s a))
buildWain truncateGenome n = do
  a <- getAndExpress
  b <- getAndExpress
  m <- getAndExpress
  g <- if truncateGenome then consumed2 else copy2
  return $ Wain n <$> a <*> b <*> m <*> pure C.initialCondition
      <*> pure 0 <*> pure 0 <*> pure Nothing <*> pure g

randomWain
  :: (RandomGen g, Pattern p, Metric p ~ UIDouble, Eq a, Random a, Genetic p, Genetic a)
    => String -> p -> Word8 -> [p] -> Word8 -> Word16 -> Rand g (Wain p a)
randomWain n app maxClassifierSize ps maxDeciderSize maxAgeOfMaturity
  = do
    b <- B.randomBrain maxClassifierSize ps maxDeciderSize
    m <- getRandomR (0,maxAgeOfMaturity)
    c <- randomInitial
    let strawMan = Wain n app b m c 0 0 Nothing ([], [])
    let g = write strawMan
    return $ Wain n app b m c 0 0 Nothing (g, g)

adjustEnergy :: UIDouble -> Wain s a -> Wain s a
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

tryMating
  :: (Genetic s, Genetic a, Diploid s, Diploid a, Eq a, Pattern s,
    Metric s ~ UIDouble)
    => Wain s a -> Wain s a -> UIDouble -> UIDouble
    -> StateT (SimpleUniverse (Wain s a)) IO [Wain s a]
tryMating me other flirtingEnergyDelta matingEnergyDelta =
  flirt me' other matingEnergyDelta
  where me' = adjustEnergy flirtingEnergyDelta $ me

flirt
  :: (Genetic s, Genetic a, Diploid s, Diploid a, Eq a, Pattern s,
    Metric s ~ UIDouble)
    => Wain s a -> Wain s a -> UIDouble
    -> StateT (SimpleUniverse (Wain s a)) IO [Wain s a]
flirt me other matingEnergyDelta
  | hasChild me    = do
      writeToLog $ agentId me ++ " is already rearing a child"
      return [me]
  | hasChild other = do
      writeToLog $ agentId me ++ " is already rearing a child"
      return [me]
  | otherwise      = mate me other matingEnergyDelta

mate
  :: (Genetic s, Genetic a, Diploid s, Diploid a, Eq a, Pattern s,
    Metric s ~ UIDouble)
    => Wain s a -> Wain s a -> UIDouble
    -> StateT (SimpleUniverse (Wain s a)) IO [Wain s a]
mate me other matingEnergyDelta = do
  writeToLog $ agentId me ++ " mates with " ++ agentId other
  let me' = coolPassion . adjustEnergy matingEnergyDelta $ me
  let other' = coolPassion . adjustEnergy matingEnergyDelta $ other
  babyName <- genName
  result <- liftIO $ evalRandIO (makeOffspring me other babyName)
  case result of
    Right baby -> do
      writeToLog $ agentId me ++ " and " ++ agentId other ++ " produce "
        ++ babyName
      return [me', other', baby]
    Left msgs -> do
      writeToLog $ "child of " ++ agentId me ++ " and " ++ agentId other
        ++ " not viable: " ++ show msgs
      return [me', other']

-- TODO Consider using State monad and/or fmaps to streamline the
-- functions below

weanChildIfReady :: Wain s a -> StateT (SimpleUniverse (Wain s a)) IO [Wain s a]
weanChildIfReady w =
  case (child w) of
    Just c  -> if age c >= ageOfMaturity c
                  then do
                    writeToLog $ agentId c ++ "has been weaned"
                    return [w {child=Nothing}, c]
                  else return [w]
    Nothing -> return [w]

chooseAction :: (Pattern p, Metric p ~ UIDouble, Eq a, Enum a, Bounded a)
    => p -> p -> Wain p a -> (Label, a, Wain p a)
chooseAction p1 p2 w = (l, a, w4)
  where (l, _, s, w2) = assessSituation p1 p2 w
        (a, w3) = chooseAction' s w2
        w4 = case (child w) of
                Just c  -> w3 { child = Just . teachAction1 p1 p2 a $ c }
                Nothing -> w3

assessSituation
  :: (Pattern p, Metric p ~ UIDouble)
    => p -> p -> Wain p a -> (Label, Label, S.Scenario, Wain p a)
assessSituation p1 p2 w = (l1, l2, sc, w{brain=b})
  where (l1, l2, sc, b) = B.assessSituation p1 p2 (condition w) (brain w)

chooseAction'
  :: (Pattern p, Metric p ~ UIDouble, Eq a, Enum a, Bounded a)
    => S.Scenario -> Wain p a -> (a, Wain p a)
chooseAction' s w = (a, w{brain=b})
  where (a, b) = B.recommendAction s (brain w)

teachAction1 :: (Pattern p, Metric p ~ UIDouble, Eq a, Enum a, Bounded a)
    => p -> p -> a -> Wain p a -> Wain p a
teachAction1 p1 p2 a w = w' { brain = B.learnAction s a (brain w) }
  where (_, _, s, w') = assessSituation p1 p2 w

classify :: (Pattern p, Metric p ~ UIDouble, Eq a, Enum a, Bounded a)
    => p -> Wain p a -> (Label, Wain p a)
classify p w = (l, w{brain=b})
  where (l, b) = B.classify p (brain w)
        
teachLabel :: Pattern p => p -> Label -> Wain p a -> Wain p a
teachLabel p l w =
  case (child w) of
    Just c  -> w' { child = Just . teachLabel1 p l $ c }
    Nothing -> w'
  where w' = teachLabel1 p l w

teachLabel1 :: Pattern p => p -> Label -> Wain p a -> Wain p a
teachLabel1 p l w = w { brain=B.learnLabel p l (brain w) }

feedback
  :: (Pattern p, Metric p ~ UIDouble, Eq a)
    => UIDouble -> Wain p a -> Wain p a
feedback deltaHappiness w =
  case (child w) of
    Just c  -> w' { child = Just . feedback1 deltaHappiness $ c }
    Nothing -> w'
  where w' = feedback1 deltaHappiness w

feedback1
  :: (Pattern p, Metric p ~ UIDouble, Eq a)
    => UIDouble -> Wain p a -> Wain p a
feedback1 deltaHappiness w
  = w { brain=B.feedback deltaHappiness (brain w) }

numberOfClassifierModels
  :: (Pattern p, Metric p ~ UIDouble)
    => Wain p a -> Int
numberOfClassifierModels = B.numberOfClassifierModels . brain

numberOfDeciderModels
  :: (Pattern p, Metric p ~ UIDouble, Eq a)
    => Wain p a -> Int
numberOfDeciderModels = B.numberOfDeciderModels . brain
