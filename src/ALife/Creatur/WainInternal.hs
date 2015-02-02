------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.WainInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module ALife.Creatur.WainInternal where

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
import qualified ALife.Creatur.Wain.Condition as C
import qualified ALife.Creatur.Wain.Brain as B
import ALife.Creatur.Wain.GeneticSOM (Label)
import qualified ALife.Creatur.Wain.Response as R
import ALife.Creatur.Wain.Statistics (Statistical, stats, iStat, dStat)
import ALife.Creatur.Wain.Util (scaleToWord8, scaleFromWord8,
  unitInterval, enforceRange)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Lens
import Control.Monad.Random (Rand, RandomGen)
import qualified Data.ByteString as BS
import Data.Datamining.Pattern (Pattern(..), Metric)
import Data.List (partition)
import Data.Serialize (Serialize, encode)
import Data.Word (Word8, Word16)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Paths_creatur_wains (version)

-- | Returns the current version number of this library.
programVersion :: String
programVersion = "creatur-wains-" ++ showVersion version

data Wain p a = Wain
  {
    -- | Each wain should have a unique name.
    _name :: String,
    -- | A wain's appearance is a pattern by which other wains can
    --   recognise it as a fellow wain.
    _appearance :: p,
    -- | The wain's brain, which recognises patterns and makes
    --   decisions.
    _brain :: B.Brain p a,
    -- | The amount of energy the wain will give to offspring at birth.
    --   This is a number between 0 and 1, inclusive.
    _devotion :: Double,
    -- | The age at which this wain will/has left its parent.
    _ageOfMaturity :: Word16,
    -- | The amount that a wain's passion increases at each CPU turn.
    --   this influences the frequency of mating.
    _passionDelta :: Double,
    -- | The wain's current energy level.
    --   This is a number between 0 and 1, inclusive.
    _energy :: Double,
    -- | The wain's current passion level
    --   This is a number between 0 and 1, inclusive.
    _passion :: Double,
    -- | The wain's current age.
    _age :: Word16,
    -- | The children this wain is currently rearing.
    _litter :: [Wain p a],
    -- | The number of children this wain has borne.
    _childrenBorneLifetime :: Word16,
    -- | The number of children this wain has reared to maturity.
    _childrenWeanedLifetime :: Word16,
    -- | The number of classifications agreed upon during a wain's
    --   lifetime.
    _swagger :: Word16,
    -- | The wain's genes.
    _genome :: ([Word8],[Word8]),
    -- The size of this wain. Useful for determining a metabolism rate.
    _wainSize :: Int
  } deriving (Eq, Generic)
makeLenses ''Wain

buildWain
  :: (Pattern p, Metric p ~ Double, Serialize a, Serialize p, Eq a)
    => String -> p -> B.Brain p a -> Double -> Word16 -> Double
      -> (Sequence, Sequence) -> Wain p a
buildWain n a b d m p g = set wainSize s w
  -- We first set the size to 0, then figure out what the size really
  -- is.
  where w = Wain
              {
                _name = n, _appearance = a, _brain = b, _devotion = d,
                _ageOfMaturity = m, _passionDelta = p, _energy = 0,
                _passion = 1, _age = 0, _litter = [],
                _childrenBorneLifetime = 0, _childrenWeanedLifetime = 0,
                _swagger = 0, _genome = g, _wainSize = 0
              }
        s = BS.length . encode $ w

-- | Constructs a wain with the specified parameters, with the
--   corresponding (generated) genome. This would normally be used only
--   for generating the initial population.
buildWainAndGenerateGenome
  :: (Pattern p, Metric p ~ Double, Serialize a, Serialize p, Genetic p,
      Genetic a, Eq a)
        => String -> p -> B.Brain p a -> Double -> Word16 -> Double -> Wain p a
buildWainAndGenerateGenome n a b d m p = set genome (g,g) strawMan
  where strawMan = buildWain n a b d m p ([], [])
        g = write strawMan

-- | Constructs a wain from its genome. This is used when a child is
--   produced as the result of mating.
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

deriving instance (Pattern p, Show p, Show (Metric p), Ord (Metric p), 
  Show a, Eq a)
    => Show (Wain p a)
 -- where
 --  show (Wain n a b c g) = "Wain " ++ n ++ " (" ++ show a ++ ") ("
 --    ++ show b ++ ") (" ++ show c ++ ") (" ++ show g ++ ")"

instance Record (Wain p a) where
  key = view name

instance SizedRecord (Wain p a) where
  size = view wainSize

instance (Pattern p, Metric p ~ Double, Eq a) =>
  Statistical (Wain p a) where
  stats w =
    iStat "age" (_age w)
      : dStat "devotion" (_devotion w)
      : iStat "maturity" (_ageOfMaturity w)
      : dStat "Δp" (_passionDelta w)
      : iStat "size" (_wainSize w)
      : iStat "children borne (lifetime)" (_childrenBorneLifetime w)
      : iStat "children reared (lifetime)" (_childrenWeanedLifetime w)
      : dStat "adult energy" e
      : dStat "child energy" ec
      : dStat "energy" (e + ec)
      : dStat "passion" (_passion w)
      : iStat "current litter size" (length . _litter $ w)
      : dStat "happiness" (happiness w)
      : iStat "swagger" (_swagger w)
      : stats (_brain w)
      ++ [iStat "genome length" ( (length . fst . _genome $ w)
                                  + (length . snd . _genome $ w) )]
    where e = _energy w
          ec = sum . map (view energy) $ _litter w
                               
instance (Serialize p, Serialize a, Pattern p, Metric p ~ Double, Eq a)
  => Serialize (Wain p a)

-- This implementation is useful for generating the genes in the
-- initial population, and for testing
instance (Genetic p, Genetic a, Pattern p, Metric p ~ Double, Eq a,
  Serialize p, Serialize a)
      => Genetic (Wain p a) where
  put w = put (_appearance w)
            >> put (_brain w)
            >> put (scaleToWord8 unitInterval . _devotion $ w)
            >> put (_ageOfMaturity w)
            >> put (scaleToWord8 unitInterval . _passionDelta $ w)
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
    where a = express (_appearance x)    (_appearance y)
          b = express (_brain x)         (_brain y)
          d = express (_devotion x)      (_devotion y)
          m = express (_ageOfMaturity x) (_ageOfMaturity y)
          p = express (_passionDelta x)  (_passionDelta y)
          
instance Agent (Wain p a) where
  agentId = view name
  isAlive w = _energy w > 0

instance (Genetic p, Genetic a, Diploid p, Diploid a, Eq a, Pattern p,
         Metric p ~ Double, Serialize p, Serialize a)
         => Reproductive (Wain p a) where
  type Strand (Wain p a) = Sequence
  produceGamete a =
    repeatWithProbability 0.1 randomCrossover (_genome a) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build n = runDiploidReader (buildWainFromGenome False n)

-- | Returns the total energy of all children in the litter.
childEnergy :: Wain p a -> Double
childEnergy = sum . map (view energy) . view litter

-- | Returns @True@ if a wain is currently raising children; returns
--   @False@ otherwise.
hasLitter :: Wain p a -> Bool
hasLitter = not . null . view litter

-- | Returns the number of children that a wain is currently raising.
litterSize :: Wain p a -> Int
litterSize = length . view litter

-- | Returns @True@ if the wain is mature; returns @False@ otherwise.
mature :: Wain p a -> Bool
mature a = _age a >= _ageOfMaturity a

-- | Returns the wain's current condition. This is useful for making
--   decisions.
condition :: Wain p a -> C.Condition
condition w = C.Condition (_energy w) (_passion w)
                (fromIntegral . length . _litter $ w)

-- | Returns the wain's current happiness level.
--   This is a number between 0 and 1, inclusive.
happiness :: Wain p a -> Double
happiness = C.happiness . condition

data Object p a = DObject p String | AObject (Wain p a)

-- | Returns the identity of an object that might be shown to a wain.
--   This should only be used for logging.
--   The wain should not have access to this information.
identity :: Object s a -> String
identity (DObject _ s) = "Image " ++ s
identity (AObject a) = _name a

-- | Returns the appearance of an object that might be shown to a wain.
appearanceOf :: Object s a -> s
appearanceOf (DObject img _) = img
appearanceOf (AObject a) = _appearance a

-- | Presents two stimuli (direct object and indirect object)
--   to a wain, and returns the the action it chooses to take,
--   and the updated wain.
chooseAction
  :: (Pattern p, Metric p ~ Double, Eq a, Enum a, Bounded a)
    => p -> p -> Wain p a
      -> (R.Response a, Wain p a, [(R.Response a, Label)])
chooseAction p1 p2 w = (r, w', xs)
  where (r, b', xs) = B.chooseAction (_brain w) p1 p2 (condition w)
        w' = set brain b' w

-- | Deducts a wain's metabolism cost from its energy, and do the same
--   for any children in the wain's litter.
applyMetabolismCost
  :: Double -> Double -> Double -> Wain p a -> (Wain p a, Double, Double)
applyMetabolismCost baseCost costPerByte childCostFactor w
  = (set litter childrenAfter adultAfter, adultCost, childCost)
  where (adultAfter, adultCost)
          = applyMetabolismCost1 baseCost costPerByte 1 w
        xs = map f $ _litter w
        f = applyMetabolismCost1 baseCost costPerByte childCostFactor
        childrenAfter = map fst xs
        childCost = sum . map snd $ xs

applyMetabolismCost1
  :: Double -> Double -> Double -> Wain p a -> (Wain p a, Double)
applyMetabolismCost1 baseCost costPerByte factor w = (w', delta')
  where (w', delta', _) = adjustEnergy1 delta w
        adultCost = baseCost
                      + costPerByte * fromIntegral (_wainSize w)
        delta = adultCost * factor

-- | Adjusts the energy of a wain and its children.
--   Note: A wain's energy is capped to the range [0,1].
adjustEnergy
  :: Double -> Wain p a -> (Wain p a, Double, Double)
adjustEnergy delta w =
  -- Rewards are shared with litter; penalties aren't
  if delta > 0 && hasLitter w
    then (w3, adultShare', childrensShare')
    else (w4, delta', 0)
  where
      childrensShare = _devotion w * delta
      (w2, childrensShare', leftover)
        = adjustChildrensEnergy delta w
      adultShare = delta - childrensShare + leftover
      (w3, adultShare', _) = adjustEnergy1 adultShare w2
      (w4, delta', _) = adjustEnergy1 delta w

adjustChildrensEnergy
  :: Double -> Wain p a -> (Wain p a, Double, Double)
adjustChildrensEnergy delta w
  = (set litter childrenAfter w, delta', leftover)
  where result = map (adjustEnergy1 delta) (_litter w)
        childrenAfter = map (\(x, _, _) -> x) result
        delta' = sum . map (\(_, y, _) -> y) $ result
        leftover = sum . map (\(_, _, z) -> z) $ result

adjustEnergy1
  :: Double -> Wain p a -> (Wain p a, Double, Double)
adjustEnergy1 delta w = (wAfter, delta', leftover)
  where eBefore = _energy w
        eAfter = max 0 . min 1 $ _energy w + delta
        wAfter = set energy eAfter w
        delta' = eAfter - eBefore
        leftover = delta - delta'

-- | Adjusts the wain's passion by the genetically-determined amount.
--   Note: The passion is capped to the range [0,1]. The litter is not
--   affected.
adjustPassion :: Wain p a -> Wain p a
adjustPassion w = set passion p w
  where p = enforceRange unitInterval (_passion w + _passionDelta w)

-- | Resets the wain's passion to zero.
--   This would normally be called immediately after mating.
coolPassion :: Wain p a -> Wain p a
coolPassion = set passion 0

-- | Increments the age of the wain, and its litter (if any).
incAge :: Wain p a -> Wain p a
incAge = incAge1 . incLitterAge

incLitterAge :: Wain p a -> Wain p a
incLitterAge w = set litter litter' w
  where litter' = map incAge1 $ _litter w
  
incAge1 :: Wain p a -> Wain p a
incAge1 = age +~ 1

-- | Increments a wain's swagger count
incSwagger :: Wain p a -> Wain p a
incSwagger = swagger +~ 1

-- | Causes a wain to considers whether it is happier or not as a
--   result of the last action it took, and modifies its decision models
--   accordingly. The wain's litter, if any, will not have access to
--   the parent's internal condition, and their own condition will not
--   change, so they do not have any way to assess whether the outcome
--   of the action was good. Instead they will simply assume that
--   the action was perfect (increased happiness by 1)
--   TODO: Do something more realistic.
reflect
  :: (Pattern p, Metric p ~ Double, Eq a)
    => p -> p -> R.Response a -> Wain p a -> (Wain p a, Double)
reflect p1 p2 r w = (set litter litter' w', err)
  where (w', err) = reflect1 r w
        a = R._action r
        litter' = map (imprint p1 p2 a) (_litter w)

reflect1
  :: (Pattern p, Metric p ~ Double, Eq a)
    => R.Response a -> Wain p a -> (Wain p a, Double)
reflect1 r w = (set brain b' w, err)
  where (b', err) = B.reflect (_brain w) r (condition w)

-- | Teaches the wain that the last action taken was a good one.
--   This can be used to help children learn by observing their parents.
imprint
  :: (Pattern p, Metric p ~ Double, Eq a)
    => p -> p -> a -> Wain p a -> Wain p a
imprint p1 p2 a w = set brain b' w
  where b' = B.imprint (_brain w) p1 p2 a (condition w)

-- | Attempts to mate two wains.
--   If either of the wains already has a litter, mating will not occur.
--   If mating does occur, the passion level of both wains will be
--   reset to zero.
--   Returns the (possibly modified) wains, together with a boolean
--   indicating whether or not mating occurred.
mate
  :: (RandomGen r, Pattern p, Metric p ~ Double, Diploid p, Diploid a,
    Genetic p, Genetic a, Eq a, Serialize p, Serialize a)
    => Wain p a -> Wain p a -> String
      -> Rand r ([Wain p a], [String], Double, Double)
mate a b babyName
  | hasLitter a
      = return ([a, b], [_name a ++ " already has a litter"], 0, 0)
  | hasLitter b
      = return ([a, b], [_name b ++ " already has a litter"], 0, 0)
  | otherwise = mate' a b babyName

mate'
  :: (RandomGen r, Pattern p, Metric p ~ Double, Diploid p, Diploid a,
    Genetic p, Genetic a, Eq a, Serialize p, Serialize a)
      => Wain p a -> Wain p a -> String
        -> Rand r ([Wain p a], [String], Double, Double)
mate' a b babyName = do
  let a2 = coolPassion a
  let b2 = coolPassion b
  result <- makeOffspring a b babyName
  case result of
    Right baby ->
      if B.brainOK (_brain baby)
        then do
          let (a3, b3, baby3, aContribution, bContribution)
                 = donateParentEnergy a2 b2 baby
          let a4 = (litter .~ [baby3])
                     . (childrenBorneLifetime +~ 1) $ a3
          return ([a4, b3], [], aContribution, bContribution)
        else return ([a2, b2], ["Child had an invalid brain"], 0, 0)
    Left msgs -> return ([a2, b2], msgs, 0, 0)

donateParentEnergy
  :: Wain p a -> Wain p a -> Wain p a
     -> (Wain p a, Wain p a, Wain p a, Double, Double)
donateParentEnergy a b c = (a', b', c', aContribution', bContribution')
  where aContribution = - _devotion a * _energy a
        bContribution = - _devotion b * _energy b
        (a', aContribution', _) = adjustEnergy1 aContribution a
        (b', bContribution', _) = adjustEnergy1 bContribution b
        cContribution = -(aContribution' + bContribution')
        (c', _, _) = adjustEnergy1 cContribution c

-- | Removes any mature children from the wain's litter.
--   Returns a list containing the (possibly modified) wain, together
--   with any children that have just been weaned.
weanMatureChildren :: Wain p a -> [Wain p a]
weanMatureChildren a =
  if null (_litter a)
    then [a]
    else a':weanlings
  where (weanlings, babes) = partition mature (_litter a)
        newWeanlings = fromIntegral $ length weanlings
        a' = (litter .~ babes)
               . (childrenWeanedLifetime +~ newWeanlings) $ a

pruneDeadChildren :: Wain p a -> [Wain p a]
pruneDeadChildren a =
  if null (_litter a)
    then [a]
    else a':deadChildren
  where (aliveChildren, deadChildren) = partition isAlive (_litter a)
        a' = set litter aliveChildren a
