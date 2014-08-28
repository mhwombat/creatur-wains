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
    buildWainAndGenerateGenome,
    buildWainFromGenome,
    -- randomWain,
    adjustEnergy,
    adjustPassion,
    coolPassion,
    happiness,
    identity,
    appearanceOf,
    hasLitter,
    litterSize,
    incAge,
    condition,
    chooseAction,
    classify,
    teachLabel,
    reflect,
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
import ALife.Creatur.Wain.Statistics (Statistical, stats, iStat, dStat)
import ALife.Creatur.Wain.Util (scaleToWord8, scaleFromWord8,
  unitInterval, enforceRange)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.State.Lazy (StateT)
import qualified Data.ByteString as BS
import Data.Datamining.Pattern (Pattern(..), Metric)
import Data.List (partition)
import Data.Maybe (fromJust)
import Data.Serialize (Serialize, encode)
import Data.Word (Word8, Word16)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Paths_creatur_wains (version)
import Text.Printf (printf)

-- | Returns the current version number of this library.
programVersion :: String
programVersion = "creatur-wains-" ++ showVersion version

data Wain p a = Wain
  {
    -- | Each wain should have a unique name.
    name :: String,
    -- | A wain's appearance is a pattern by which other wains can
    --   recognise it as a fellow wain.
    appearance :: p,
    -- | The wain's brain, which recognises patterns and makes
    --   decisions.
    brain :: B.Brain p a,
    -- | The amount of energy the wain will give to offspring at birth.
    --   This is a number between 0 and 1, inclusive.
    devotion :: Double,
    -- | The age at which this wain will/has left its parent.
    ageOfMaturity :: Word16,
    -- | The amount that a wain's passion increases at each CPU turn.
    --   this influences the frequency of mating.
    passionDelta :: Double,
    -- | The wain's current energy level.
    --   This is a number between 0 and 1, inclusive.
    energy :: Double,
    -- | The wain's current passion level
    --   This is a number between 0 and 1, inclusive.
    passion :: Double,
    -- | The wain's current age.
    age :: Word16,
    -- | The children this wain is currently rearing.
    litter :: [Wain p a],
    -- | The number of children this wain has borne.
    childrenBorneLifetime :: Word16,
    -- | The number of children this wain has reared to maturity.
    childrenWeanedLifetime :: Word16,
    -- | The wain's genes.
    genome :: ([Word8],[Word8]),
    -- The size of this wain. Useful for determining a metabolism rate.
    wainSize :: Int
  } deriving (Eq, Generic)

buildWain
  :: (Pattern p, Metric p ~ Double, Serialize a, Serialize p, Eq a)
    => String -> p -> B.Brain p a -> Double -> Word16 -> Double -> (Sequence, Sequence)
      -> Wain p a
buildWain n a b d m p g = w { wainSize = s }
  -- We first set the size to 0, then figure out what the size really
  -- is.
  where w = Wain
              {
                name = n, appearance = a, brain = b, devotion = d,
                ageOfMaturity = m, passionDelta = p, energy = 0,
                passion = 0, age = 0, litter = [],
                childrenBorneLifetime = 0, childrenWeanedLifetime = 0,
                genome = g, wainSize = 0
              }
        s = BS.length . encode $ w

buildWainAndGenerateGenome
  :: (Pattern p, Metric p ~ Double, Serialize a, Serialize p, Genetic p,
      Genetic a, Eq a)
        => String -> p -> B.Brain p a -> Double -> Word16 -> Double -> Wain p a
buildWainAndGenerateGenome n a b d m p = strawMan { genome=(g,g) }
  where strawMan = buildWain n a b d m p ([], [])
        g = write strawMan

deriving instance (Pattern p, Show p, Show (Metric p), Ord (Metric p), 
  Show a, Eq a)
    => Show (Wain p a)
 -- where
 --  show (Wain n a b c g) = "Wain " ++ n ++ " (" ++ show a ++ ") ("
 --    ++ show b ++ ") (" ++ show c ++ ") (" ++ show g ++ ")"

instance Record (Wain p a) where
  key = name

instance SizedRecord (Wain p a) where
  size = wainSize

instance (Pattern p, Metric p ~ Double) => Statistical (Wain p a) where
  stats w =
    iStat "age" (age w)
      : dStat "devotion" (devotion w)
      : iStat "maturity" (ageOfMaturity w)
      : dStat "Δp" (passionDelta w)
      : iStat "size" (wainSize w)
      : iStat "children borne (lifetime)" (childrenBorneLifetime w)
      : iStat "children reared (lifetime)" (childrenWeanedLifetime w)
      : dStat "energy" (energy w)
      : dStat "passion" (passion w)
      : iStat "current litter size" (length $ litter w)
      : dStat "happiness" (happiness w)
      : stats (brain w)
      ++ [iStat "genome length" ( (length . fst $ genome w)
                                  + (length . snd $ genome w) )]
                               
instance (Serialize p, Serialize a, Pattern p, Metric p ~ Double, Eq a)
  => Serialize (Wain p a)

-- This implementation is useful for generating the genes in the
-- initial population, and for testing
instance (Genetic p, Genetic a, Pattern p, Metric p ~ Double, Eq a,
  Serialize p, Serialize a)
      => Genetic (Wain p a) where
  put w = put (appearance w)
            >> put (brain w)
            >> put (scaleToWord8 unitInterval $ devotion w)
            >> put (ageOfMaturity w)
            >> put (scaleToWord8 unitInterval $ passionDelta w)
  get = do
    g <- copy
    a <- get
    b <- get
    d <- fmap (fmap (scaleFromWord8 unitInterval)) get
    m <- get
    p <- fmap (fmap (scaleFromWord8 unitInterval)) get
    return $ buildWain "" <$> a <*> b <*> d <*> m <*> p <*> pure (g, g)

-- This implementation is useful for testing
instance (Diploid p, Diploid a, Pattern p, Metric p ~ Double, Eq a,
  Serialize p, Serialize a)
      => Diploid (Wain p a) where
  express x y = buildWain "" a b d m p ([],[])
    where a = express (appearance x) (appearance y)
          b = express (brain x) (brain y)
          d = express (devotion x) (devotion y)
          m = express (ageOfMaturity x) (ageOfMaturity y)
          p = express (passionDelta x) (passionDelta y)
          
instance Agent (Wain p a) where
  agentId = name
  isAlive w = energy w > 0

instance (Genetic p, Genetic a, Diploid p, Diploid a, Eq a, Pattern p,
         Metric p ~ Double, Serialize p, Serialize a)
         => Reproductive (Wain p a) where
  type Strand (Wain p a) = Sequence
  produceGamete a =
    repeatWithProbability 0.1 randomCrossover (genome a) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build n = runDiploidReader (buildWainFromGenome False n)

buildWainFromGenome
  :: (Genetic p, Genetic a, Diploid p, Diploid a, Eq a, Pattern p,
    Metric p ~ Double, Serialize p, Serialize a)
    => Bool -> String -> DiploidReader (Either [String] (Wain p a))
buildWainFromGenome truncateGenome n = do
  a <- getAndExpress
  b <- getAndExpress
  d <- getAndExpress
  m <- getAndExpress
  p <- getAndExpress
  let d' = fmap (scaleFromWord8 unitInterval) d
  let p' = fmap (scaleFromWord8 unitInterval) p
  g <- if truncateGenome then consumed2 else copy2
  return $ buildWain n <$> a <*> b <*> d' <*> m <*> p' <*> pure g

-- randomWain
--   :: (RandomGen g, Pattern p, Metric p ~ Double, Genetic p, Random p,
--       Serialize p, Random a, Genetic a, Serialize a, Eq a)
--     => String -> p -> [p] -> RandomDecayingGaussianParams
--       -> RandomDecayingGaussianParams -> Word16 -> Rand g (Wain p a)
-- randomWain n app ps classifierParams deciderParams maxAgeOfMaturity = do
--     fc <- randomDecayingGaussian classifierParams
--     let c = buildGeneticSOM (sideLength classifierParams) fc ps
--     fd <- randomDecayingGaussian deciderParams
--     xs <- sequence . repeat $ R.randomResponse (numModels c) 
--     let d = buildGeneticSOM (sideLength deciderParams) fd xs
--     let b = B.buildBrain c d
--     m <- getRandomR (0,maxAgeOfMaturity)
--     p <- getRandomR unitInterval
--     return $ buildWainAndGenerateGenome n app b m p

data Object p a = DObject p String | AObject (Wain p a)

-- | Returns the identity of an object that might be shown to a wain.
--   This should only be used for logging.
--   The wain should not have access to this information.
identity :: Object s a -> String
identity (DObject _ s) = "Image " ++ s
identity (AObject a) = name a

-- | Returns the appearance of an object that might be shown to a wain.
appearanceOf :: Object s a -> s
appearanceOf (DObject img _) = img
appearanceOf (AObject a) = appearance a

-- TODO Consider using State monad and/or fmaps to streamline the
-- functions below

-- | Presents two stimuli (direct object and indirect object)
--   to a wain, and returns the the action it chooses to take,
--   and the updated wain.
chooseAction
  :: (Pattern p, Metric p ~ Double, U.Universe u, Eq a, Enum a,
    Bounded a, Show a)
    => p -> p -> Wain p a
      -> StateT u IO (R.Response a, Wain p a)
chooseAction p1 p2 w = do
  let n = name w
  let (r, b', xs) = B.chooseAction (brain w) p1 p2 (condition w)
  U.writeToLog $ n ++ "'s assessment=" ++ pretty (R.scenario r)
  describeModels w
  describeOutcomes w xs
  U.writeToLog $ n ++ " decides to " ++ show (R.action r)
  return (r, w {brain=b'})

describeModels :: (U.Universe u, Show a, Eq a) => Wain p a -> StateT u IO ()
describeModels w = mapM_ (U.writeToLog . f) ms
  where ms = toList . B.decider $ brain w
        f (l, r) = name w ++ "'s decider model " ++ show l ++ "="
                     ++ pretty r

describeOutcomes
  :: (U.Universe u, Show a)
    => Wain p a -> [(R.Response a, Label)] -> StateT u IO ()
describeOutcomes w xs = mapM_ (U.writeToLog . f) xs
  where f (r, l) = name w ++ "'s predicted outcome of "
                     ++ show (R.action r) ++ " is "
                     ++ (printf "%.3f" . fromJust . R.outcome $ r)
                     ++ " from model " ++ show l

-- | Returns @True@ if a wain is currently raising children; returns
--   @False@ otherwise.
hasLitter :: Wain p a -> Bool
hasLitter = not . null . litter

-- | Returns the number of children that a wain is currently raising.
litterSize :: Wain p a -> Int
litterSize = length . litter

-- | Returns @True@ if the wain is mature; returns @False@ otherwise.
mature :: Wain p a -> Bool
mature a = age a >= ageOfMaturity a

-- | Returns the wain's current condition. This is useful for making
--   decisions.
condition :: Wain p a -> C.Condition
condition w = C.Condition (energy w) (passion w)
                (fromIntegral . length . litter $ w)

-- | Returns the wain's current happiness level.
--   This is a number between 0 and 1, inclusive.
happiness :: Wain p a -> Double
happiness = C.happiness . condition

-- | Adjusts the wain's energy by the amount specified.
--   Note: The energy is capped to the range [0,1].
adjustEnergy :: Double -> Wain p a -> Wain p a
adjustEnergy delta w = w {energy=e}
  -- where e = enforceRange unitInterval (energy w + delta)
  where e = min 1 (energy w + delta)

-- | Adjusts the wain's passion by the genetically-determined amount.
--   Note: The passion is capped to the range [0,1].
adjustPassion :: Wain p a -> Wain p a
adjustPassion w = w {passion=p}
  where p = enforceRange unitInterval (passion w + passionDelta w)

-- | Resets the wain's passion to zero.
--   This would normally be called immediately after mating.
coolPassion :: Wain p a -> Wain p a
coolPassion w = w {passion=0}

-- | Increments the age of the wain, and its litter (if any).
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
  
-- | Presents a stimulus to a wain.
--   Returns the index (grid location) of the model that most closely
--   matches the input pattern,
--   and the updated wain.
classify
  :: (Pattern p, Metric p ~ Double)
    => p -> Wain p a -> (Label, Wain p a)
classify p w = (l, w{brain=b})
  where (l, b) = B.classify p (brain w)

-- | Teaches a pattern + label to a wain, and its litter (if any).
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
  U.writeToLog $ name w ++ " learns to (maybe) label this " ++ show l
  return $ w { brain=B.learnLabel p l (brain w) }

-- | Causes a wain to considers whether it is happier or not as a
--   result of the last action it took, and modifies its decision models
--   accordingly. The wain's litter, if any, will not have access to
--   the parent's internal condition, and their own condition will not
--   change, so they do not have any way to assess whether the outcome
--   of the action was good. Instead they will simply assume that
--   the action was perfect (increased happiness by 1)
--   TODO: Do something more realistic.
reflect
  :: (Pattern p, Metric p ~ Double, Eq a, U.Universe u)
    => p -> p -> R.Response a -> Wain p a -> StateT u IO (Wain p a, Double)
reflect p1 p2 r w = do
  (w', err) <- reflect1 r w
  let a = R.action r
  litter' <- mapM (imprint p1 p2 a) (litter w)
  return (w' { litter=litter' }, err)

reflect1
  :: (Pattern p, Metric p ~ Double, Eq a, U.Universe u)
    => R.Response a -> Wain p a -> StateT u IO (Wain p a, Double)
reflect1 r w = do
  U.writeToLog $ name w ++ " is reflecting on the outcome"
  let (b', err) = B.reflect (brain w) r (condition w)
  return (w { brain=b'}, err)

-- | Teaches the wain that the last action taken was a good one.
--   This can be used to help children learn by observing their parents.
imprint
  :: (Pattern p, Metric p ~ Double, Eq a, U.Universe u)
    => p -> p -> a -> Wain p a -> StateT u IO (Wain p a)
imprint p1 p2 a w = do
  U.writeToLog $ name w ++ " learns this action"
  return $ w { brain=B.imprint (brain w) p1 p2 a }

-- | Attempts to mate two wains.
--   If either of the wains already has a litter, mating will not occur.
--   If mating does occur, the passion level of both wains will be
--   reset to zero.
--   Returns the (possibly modified) wains, together with a boolean
--   indicating whether or not mating occurred.
tryMating
  :: (U.Universe u, Pattern p, Metric p ~ Double, Diploid p, Diploid a,
    Genetic p, Genetic a, Eq a, Serialize p, Serialize a)
    => Wain p a -> Wain p a -> StateT u IO ([Wain p a], Bool)
tryMating a b
  | hasLitter a = do
      U.writeToLog $ name a ++ " already has a litter"
      return ([a, b], False)
  | hasLitter b = do
      U.writeToLog $ name b ++ " already has a litter"
      return ([a, b], False)
  | otherwise   = do
      U.writeToLog $ name a ++ " mates with " ++ name b
      as <- mate a b
      return (as, True)

mate
  :: (U.Universe u, Pattern p, Metric p ~ Double, Diploid p, Diploid a,
    Genetic p, Genetic a, Eq a, Serialize p, Serialize a)
      => Wain p a -> Wain p a -> StateT u IO [Wain p a]
mate a b = do
  let a2 = coolPassion a
  let b2 = coolPassion b
  babyName <- U.genName
  result <- liftIO $ evalRandIO (makeOffspring a b babyName)
  case result of
    Right baby -> do
      if (B.brainOK $ brain baby)
        then do
          U.writeToLog $ name a ++ " and " ++ name b ++ " produce "
            ++ babyName
          let aContribution = (devotion a)*(energy a)
          let bContribution = (devotion b)*(energy b)
          let a3 = adjustEnergy (-aContribution) a2
          let b3 = adjustEnergy (-bContribution) b2
          let baby3 = adjustEnergy (aContribution + bContribution) baby  
          return
            [a3 {litter=[baby3],
                 childrenBorneLifetime=childrenBorneLifetime a + 1}, b3]
        else do
          U.writeToLog $ "child of " ++ name a ++ " and " ++ name b
            ++ " would have had an abnormal brain"
          return [a2, b2]
    Left msgs -> do
      U.writeToLog $ "child of " ++ name a ++ " and " ++ name b
        ++ " not viable: " ++ show msgs
      return [a2, b2]

-- | Removes any mature children from the wain's litter.
--   Returns a list containing the (possibly modified) wain, together
--   with any children that have just been weaned.
weanMatureChildren
  :: U.Universe u
    => Wain p a -> StateT u IO [Wain p a]
weanMatureChildren a =
  if null (litter a)
    then return [a]
    else do
      let (weanlings, babes) = partition mature (litter a)
      mapM_ (\c -> U.writeToLog $
                    (name c) ++ " weaned from " ++ name a)
                      weanlings
      let a' = a { litter = babes,
                   childrenWeanedLifetime =
                     childrenWeanedLifetime a +
                       fromIntegral (length weanlings) }
      return $ a':weanlings
