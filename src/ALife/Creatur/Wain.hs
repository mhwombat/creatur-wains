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
    adjustPassion,
    coolPassion,
    energy,
    passion,
    identity,
    appearanceOf,
    hasLitter,
    incAge,
    chooseAction,
    classify,
    teachLabel,
    feedback,
    randomWain,
    tryMating,
    weanMatureChildren,
    programVersion
  ) where

import ALife.Creatur (Agent, agentId, isAlive)
import ALife.Creatur.Database (Record, SizedRecord, key)
import qualified ALife.Creatur.Database (size)
import ALife.Creatur.Genetics.BRGCWord8 (Genetic, DiploidReader,
  Sequence, get, put, copy, copy2, consumed2, getAndExpress,
  runDiploidReader, write)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Genetics.Recombination (mutatePairedLists,
  randomCrossover, randomCutAndSplice, randomOneOfPair,
  repeatWithProbability, withProbability)
import ALife.Creatur.Genetics.Reproduction.Sexual (Reproductive, Strand,
  produceGamete, build, makeOffspring)
import qualified ALife.Creatur.Universe as U
import qualified ALife.Creatur.Wain.Condition as C
import qualified ALife.Creatur.Wain.Brain as B
import ALife.Creatur.Wain.GeneticSOM (Label, toList)
import ALife.Creatur.Wain.Pretty (pretty)
import qualified ALife.Creatur.Wain.Response as R
import qualified ALife.Creatur.Wain.Scenario as S
import ALife.Creatur.Wain.Statistics (Statistical, stats, iStat, dStat)
import ALife.Creatur.Wain.Util (scaleToWord8, scaleFromWord8,
  unitInterval)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, getRandomR, evalRandIO)
import Control.Monad.State.Lazy (StateT)
import qualified Data.ByteString as BS
import Data.Datamining.Pattern (Pattern(..), Metric)
import Data.List (partition, maximumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Serialize (Serialize, encode)
import Data.Word (Word8, Word16)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Paths_creatur_wains (version)
import System.Random (Random)

programVersion :: String
programVersion = "creatur-wains-" ++ showVersion version

data Wain p a = Wain
  {
    name :: String,
    appearance :: p,
    brain :: B.Brain p a,
    ageOfMaturity :: Word16,
    passionDelta :: Double,
    condition :: C.Condition,
    age :: Word16,
    numberOfChildren :: Word16,
    litter :: [Wain p a],
    genome :: ([Word8],[Word8]),
    size :: Int
  } deriving (Eq, Generic)

deriving instance (Pattern p, Show p, Show (Metric p), Ord (Metric p), 
  Show a, Eq a)
    => Show (Wain p a)
 -- where
 --  show (Wain n a b c g) = "Wain " ++ n ++ " (" ++ show a ++ ") ("
 --    ++ show b ++ ") (" ++ show c ++ ") (" ++ show g ++ ")"

instance Record (Wain p a) where
  key = name

instance SizedRecord (Wain p a) where
  size = ALife.Creatur.Database.size

instance (Pattern p, Metric p ~ Double) => Statistical (Wain p a) where
  stats w =
    iStat "age" (age w)
      : iStat "maturity" (ageOfMaturity w)
      : dStat "Δp" (passionDelta w)
      : iStat "size" (size w)
      : iStat "total # of children" (numberOfChildren w)
      : iStat "current litter size" (length $ litter w)
      : stats (brain w)
      ++ stats (condition w)
      ++ [iStat "genome length" ( (length . fst $ genome w)
                                  + (length . snd $ genome w) )]
                               
instance (Serialize p, Serialize a, Pattern p, Metric p ~ Double, Eq a)
  => Serialize (Wain p a)

-- This implementation is useful for generating the genes in the
-- initial population, and for testing
instance (Genetic p, Genetic a, Pattern p, Metric p ~ Double, Eq a,
  Serialize p, Serialize a)
      => Genetic (Wain p a) where
  put w = put (appearance w) >> put (brain w) >> put (ageOfMaturity w)
    >> put (scaleToWord8 unitInterval $ passionDelta w)
  get = do
    g <- copy
    a <- get
    b <- get
    m <- get
    p <- fmap (fmap (scaleFromWord8 unitInterval)) get
    return $ buildWain2 "" <$> a <*> b <*> m <*> p <*> pure (g, g)

-- This implementation is useful for testing
instance (Diploid p, Diploid a, Pattern p, Metric p ~ Double, Eq a,
  Serialize p, Serialize a)
      => Diploid (Wain p a) where
  express x y = buildWain2 "" a b m p ([],[])
    where a = express (appearance x) (appearance y)
          b = express (brain x) (brain y)
          m = express (ageOfMaturity x) (ageOfMaturity y)
          p = express (passionDelta x) (passionDelta y)
          
instance Agent (Wain p a) where
  agentId = name
  isAlive = C.alive . condition

instance (Genetic p, Genetic a, Diploid p, Diploid a, Eq a, Pattern p,
         Metric p ~ Double, Serialize p, Serialize a)
         => Reproductive (Wain p a) where
  type Strand (Wain p a) = Sequence
  produceGamete a =
    repeatWithProbability 0.1 randomCrossover (genome a) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build n = runDiploidReader (buildWain False n)

hasLitter :: Wain p a -> Bool
hasLitter = not . null . litter

buildWain
  :: (Genetic p, Genetic a, Diploid p, Diploid a, Eq a, Pattern p,
    Metric p ~ Double, Serialize p, Serialize a)
    => Bool -> String -> DiploidReader (Either [String] (Wain p a))
buildWain truncateGenome n = do
  a <- getAndExpress
  b <- getAndExpress
  m <- getAndExpress
  p <- getAndExpress
  let p' = fmap (scaleFromWord8 unitInterval) p
  g <- if truncateGenome then consumed2 else copy2
  return $ buildWain2 n <$> a <*> b <*> m <*> p' <*> pure g

buildWain2
  :: (Pattern p, Metric p ~ Double, Serialize a, Serialize p, Eq a)
    => String -> p -> B.Brain p a -> Word16 -> Double -> (Sequence, Sequence)
      -> Wain p a
buildWain2 n a b m p g = w { size=s }
  where  w = Wain n a b m p C.initialCondition 0 0 [] g 0
         s = BS.length . encode $ w
  
randomWain
  :: (RandomGen g, Pattern p, Metric p ~ Double, Eq a, Random a,
    Genetic p, Genetic a, Serialize a, Serialize p)
      => String -> p -> Word8 -> [p] -> Word8 -> Word16 -> Rand g (Wain p a)
randomWain n app classifierSize ps deciderSize maxAgeOfMaturity
  = do
    b <- B.randomBrain classifierSize ps deciderSize
    m <- getRandomR (0,maxAgeOfMaturity)
    p <- getRandomR unitInterval
    let strawMan = buildWain2 n app b m p ([], [])
    let g = write strawMan
    return $ strawMan { genome=(g,g) }

adjustEnergy :: Double -> Wain p a -> Wain p a
adjustEnergy delta a
  = a {condition = C.adjustEnergy delta (condition a)}

adjustPassion :: Wain p a -> Wain p a
adjustPassion a
  = a {condition = C.adjustPassion delta (condition a)}
  where delta = passionDelta a

coolPassion :: Wain p a -> Wain p a
coolPassion a = a {condition = C.coolPassion (condition a)}

data Object p a = DObject p String | AObject (Wain p a)

identity :: Object s a -> String
identity (DObject _ s) = "Image " ++ s
identity (AObject a) = agentId a

appearanceOf :: Object s a -> s
appearanceOf (DObject img _) = img
appearanceOf (AObject a) = appearance a

-- TODO Consider using State monad and/or fmaps to streamline the
-- functions below

chooseAction
  :: (Pattern p, Metric p ~ Double, U.Universe u, Eq a, Enum a,
    Bounded a, Show a)
    => p -> p -> Wain p a
      -> StateT u IO (Label, a, Wain p a)
chooseAction p1 p2 w = do
  let (l, _, s, w2) = assessSituation p1 p2 w
  U.writeToLog $ "Assessment=[" ++ pretty s ++ "]"
  let ms = toList . B.decider $ brain w2
  mapM_ (U.writeToLog . describeModel ) ms
  let outcomes = predictOutcomes w2 s
  mapM_ (U.writeToLog . describeOutcome) outcomes
  let a = R.action $ bestOutcome outcomes
  w3 <- teachActionToLitter p1 p2 a w2
  return (l, a, w3)

describeModel :: Show a => (Label, R.Response a) -> String
describeModel (l, r) = "Model" ++ show l ++ "=[" ++ pretty r ++ "]"

describeOutcome :: Show a => R.Response a -> String
describeOutcome r 
  = "Predicted outcome of " ++ show (R.action r) ++ " is "
      ++ show (R.outcome r)

predictOutcomes
  :: (Bounded a, Enum a, Eq a)
     => Wain p a -> S.Scenario -> [R.Response a]
predictOutcomes w s = map (B.predict $ brain w) $ R.possibleResponses s

bestOutcome
  :: (Eq a, Enum a, Bounded a)
    => [R.Response a] -> R.Response a
bestOutcome = maximumBy comp
  where comp = comparing (fromMaybe 0 . R.outcome)

assessSituation
  :: (Pattern p, Metric p ~ Double)
    => p -> p -> Wain p a -> (Label, Label, S.Scenario, Wain p a)
assessSituation p1 p2 w = (l1, l2, sc, w{brain=b})
  where (l1, l2, sc, b)
          = B.assessSituation p1 p2 (condition w) (brain w)

teachActionToLitter
  :: (Pattern p, Metric p ~ Double, U.Universe u, Eq a, Enum a,
    Bounded a, Show a)
    => p -> p -> a -> Wain p a
      -> StateT u IO (Wain p a)
teachActionToLitter p1 p2 a w = do
  litter' <- mapM (teachAction1 p1 p2 a) (litter w)
  return $ w { litter=litter' }

teachAction1
  :: (Pattern p, Metric p ~ Double, U.Universe u, Eq a, Enum a,
    Bounded a, Show a)
    => p -> p -> a -> Wain p a -> StateT u IO (Wain p a)
teachAction1 p1 p2 a w = do
  U.writeToLog $ agentId w ++ " observes the response " ++ show a
  let (_, _, s, w') = assessSituation p1 p2 w
  return $ w' { brain = B.learnAction s a (brain w) }

incAge
  :: (Pattern p, Metric p ~ Double, U.Universe u)
    => Wain p a -> StateT u IO (Wain p a)
incAge w = incAge1 w >>= incLitterAge

incLitterAge
  :: (Pattern p, Metric p ~ Double, U.Universe u)
    => Wain p a -> StateT u IO (Wain p a)
incLitterAge w = do
  litter' <- mapM incAge1 (litter w)
  return w { litter=litter' }
  
incAge1
  :: (Pattern p, Metric p ~ Double, U.Universe u)
    => Wain p a -> StateT u IO (Wain p a)
incAge1 w = return w { age=age w + 1 }
  
classify
  :: (Pattern p, Metric p ~ Double)
    => p -> Wain p a -> (Label, Wain p a)
classify p w = (l, w{brain=b})
  where (l, b) = B.classify p (brain w)
        
teachLabel
  :: (Pattern p, Metric p ~ Double, U.Universe u)
    => p -> Label -> Wain p a -> StateT u IO (Wain p a)
teachLabel p l w = teachLabel1 p l w >>= teachLabelToLitter p l

teachLabelToLitter
  :: (Pattern p, Metric p ~ Double, U.Universe u)
    => p -> Label -> Wain p a -> StateT u IO (Wain p a)
teachLabelToLitter p l w = do
  litter' <- mapM (teachLabel1 p l) (litter w)
  return w { litter=litter' }
  
teachLabel1
  :: (Pattern p, Metric p ~ Double, U.Universe u)
    => p -> Label -> Wain p a -> StateT u IO (Wain p a)
teachLabel1 p l w = do
  U.writeToLog $ agentId w ++ " learns to (maybe) label this " ++ show l
  return $ w { brain=B.learnLabel p l (brain w) }

feedback
  :: (Pattern p, Metric p ~ Double, Eq a)
    => Double -> Wain p a -> Wain p a
feedback deltaHappiness w = w' { litter=litter' }
  where w' = feedback1 deltaHappiness w
        litter' = map (feedback1 deltaHappiness) (litter w)

feedback1
  :: (Pattern p, Metric p ~ Double, Eq a)
    => Double -> Wain p a -> Wain p a
feedback1 deltaHappiness w
  = w { brain=B.feedback deltaHappiness (brain w) }

tryMating
  :: (U.Universe u, Pattern p, Metric p ~ Double, Diploid p, Diploid a,
    Genetic p, Genetic a, Eq a, Serialize p, Serialize a)
    => Wain p a -> Wain p a -> StateT u IO ([Wain p a], Bool)
tryMating a b
  | hasLitter a = do
      U.writeToLog $ agentId a ++ " already has a litter"
      return ([a], False)
  | hasLitter b = do
      U.writeToLog $ agentId b ++ " already has a litter"
      return ([a], False)
  | otherwise   = do
      U.writeToLog $ agentId a ++ " mates with " ++ agentId b
      as <- mate a b
      return (as, True)

mate
  :: (U.Universe u, Pattern p, Metric p ~ Double, Diploid p, Diploid a,
    Genetic p, Genetic a, Eq a, Serialize p, Serialize a)
      => Wain p a -> Wain p a -> StateT u IO [Wain p a]
mate a b = do
  let a' = coolPassion a
  let b' = coolPassion b
  babyName <- U.genName
  result <- liftIO $ evalRandIO (makeOffspring a b babyName)
  case result of
    Right baby -> do
      if (B.brainOK $ brain baby)
        then do
          U.writeToLog $ agentId a ++ " and " ++ agentId b ++ " produce "
            ++ babyName
          return [a' {litter=[baby]}, b']
        else do
          U.writeToLog $ "child of " ++ agentId a ++ " and " ++ agentId b
            ++ " has an abnormal brain"
          return [a', b']
    Left msgs -> do
      U.writeToLog $ "child of " ++ agentId a ++ " and " ++ agentId b
        ++ " not viable: " ++ show msgs
      return [a', b']

weanMatureChildren
  :: U.Universe u
    => Wain p a -> StateT u IO [Wain p a]
weanMatureChildren a =
  if null (litter a)
    then return [a]
    else do
      let (weanlings, babes) = partition mature (litter a)
      mapM_ (\c -> U.writeToLog $ (agentId c) ++ " weaned") weanlings
      return $ (a { litter=babes }):weanlings

mature :: Wain p a -> Bool
mature a = age a >= ageOfMaturity a

energy :: Wain p a -> Double
energy = C.cEnergy . condition

passion :: Wain p a -> Double
passion = C.cPassion . condition
