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
import qualified ALife.Creatur.Wain.Brain as B
import qualified ALife.Creatur.Wain.Classifier as Cl
import ALife.Creatur.Wain.GeneticSOM (Tweaker, Pattern)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double)
import qualified ALife.Creatur.Wain.Predictor as P
import qualified ALife.Creatur.Wain.Response as R
import ALife.Creatur.Wain.Probability (Probability)
import ALife.Creatur.Wain.Statistics (Statistical, stats, iStat, dStat)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble,
  doubleToUI, forceDoubleToUI)
import ALife.Creatur.Wain.Util (unitInterval, enforceRange)
import Control.Lens
import Control.Monad.Random (Rand, RandomGen)
import Data.List (partition)
import Data.Serialize (Serialize)
import Data.Word (Word8, Word16)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Paths_creatur_wains (version)

-- | Returns the current version number of this library.
packageVersion :: String
packageVersion = "creatur-wains-" ++ showVersion version

data Wain p t a = Wain
  {
    -- | Each wain should have a unique name.
    _name :: String,
    -- | A wain's appearance is a pattern by which other wains can
    --   recognise it as a fellow wain.
    _appearance :: p,
    -- | The wain's brain, which recognises patterns and makes
    --   decisions.
    _brain :: B.Brain p t a,
    -- | The amount of energy the wain will give to offspring at birth.
    --   This is a number between 0 and 1, inclusive.
    _devotion :: UIDouble,
    -- | The age at which this wain will/has left its parent.
    _ageOfMaturity :: Word16,
    -- | The amount that a wain's passion increases at each CPU turn.
    --   this influences the frequency of mating.
    _passionDelta :: UIDouble,
    -- | The amount that a wain's boredom increases at each CPU turn.
    --   this influences the frequency of mating.
    _boredomDelta :: UIDouble,
    -- | The wain's current energy level.
    --   This is a number between 0 and 1, inclusive.
    _energy :: UIDouble,
    -- | The wain's current passion level
    --   This is a number between 0 and 1, inclusive.
    _passion :: UIDouble,
    -- | The wain's current boredom level
    --   This is a number between 0 and 1, inclusive.
    _boredom :: UIDouble,
    -- | The wain's current age.
    _age :: Word16,
    -- | The children this wain is currently rearing.
    _litter :: [Wain p t a],
    -- | The number of children this wain has borne.
    _childrenBorneLifetime :: Word16,
    -- | The number of children this wain has reared to maturity.
    _childrenWeanedLifetime :: Word16,
    -- | The wain's genes.
    _genome :: ([Word8],[Word8])
  } deriving (Eq, Generic)
makeLenses ''Wain

buildWain
  :: (Genetic p, Genetic t, Genetic a, Eq a, Tweaker t, p ~ Pattern t,
    Serialize p, Serialize t, Serialize a, Ord a)
    => String -> p -> B.Brain p t a -> UIDouble -> Word16 -> UIDouble
      -> UIDouble -> (Sequence, Sequence) -> Wain p t a
buildWain wName wAppearance wBrain wDevotion wAgeOfMaturity
  wPassionDelta wBoredomDelta g =
    Wain
      {
        _name = wName,
        _appearance = wAppearance,
        _brain = wBrain,
        _devotion = wDevotion,
        _ageOfMaturity = wAgeOfMaturity,
        _passionDelta = wPassionDelta,
        _boredomDelta = wBoredomDelta,
        _energy = 0,
        _passion = 1,
        _boredom = 1,
        _age = 0,
        _litter = [],
        _childrenBorneLifetime = 0,
        _childrenWeanedLifetime = 0,
        _genome = g
      }

-- updateSize
--   :: (Serialize p, Serialize t, Serialize a, Ord a, Tweaker t,
--     p ~ Pattern t)
--       => Wain p t a -> Wain p t a
-- updateSize w = w { _wainSize = BS.length . encode $ w0 }
--   where w0 = w { _litter=[] }

-- | Constructs a wain with the specified parameters, with the
--   corresponding (generated) genome. This would normally be used only
--   for generating the initial population.
buildWainAndGenerateGenome
  :: (Genetic p, Genetic t, Genetic a,
    Serialize p, Serialize t, Serialize a,
      Eq a, Ord a, Tweaker t, p ~ Pattern t)
        => String -> p -> B.Brain p t a -> UIDouble -> Word16
          -> UIDouble -> UIDouble -> Wain p t a
buildWainAndGenerateGenome wName wAppearance wBrain wDevotion
  wAgeOfMaturity wPassionDelta wBoredomDelta = set genome (g,g) strawMan
  where strawMan = buildWain wName wAppearance wBrain wDevotion
                     wAgeOfMaturity wPassionDelta wBoredomDelta ([], [])
        g = write strawMan

-- | Constructs a wain from its genome. This is used when a child is
--   produced as the result of mating.
buildWainFromGenome
  :: (Genetic p, Genetic t, Genetic a, Diploid p, Diploid t, Diploid a,
    Serialize p, Serialize t, Serialize a, Ord a, Eq a, Tweaker t,
      p ~ Pattern t)
        => Bool -> String
          -> DiploidReader (Either [String] (Wain p t a))
buildWainFromGenome truncateGenome wName = do
  wAppearance <- getAndExpress
  wBrain <- getAndExpress
  wDevotion <- getAndExpress
  wAgeOfMaturity <- getAndExpress
  wPassionDelta <- getAndExpress
  wBoredomDelta <- getAndExpress
  g <- if truncateGenome then consumed2 else copy2
  return $ buildWain wName <$> wAppearance <*> wBrain <*> wDevotion
             <*> wAgeOfMaturity <*> wPassionDelta <*> wBoredomDelta
             <*> pure g

deriving instance (Show p, Show t, Show a, Eq a)
    => Show (Wain p t a)

instance Record (Wain p t a) where
  key = view name

instance SizedRecord (Wain p t a) where
  size = const 1

instance (Eq a, Ord a) =>
  Statistical (Wain p t a) where
  stats w =
    iStat "age" (_age w)
      : stats (_brain w)
      ++ dStat "devotion" (_devotion w)
      : iStat "maturity" (_ageOfMaturity w)
      : dStat "Δp" (_passionDelta w)
      : dStat "Δb" (_boredomDelta w)
      : iStat "children borne (lifetime)" (_childrenBorneLifetime w)
      : iStat "children reared (lifetime)" (_childrenWeanedLifetime w)
      : dStat "energy" (uiToDouble e + uiToDouble ec)
      : dStat "passion" (_passion w)
      : dStat "boredom" (_boredom w)
      : dStat "happiness" (happiness w)
      : iStat "current litter size" (length . _litter $ w)
      : iStat "children borne (lifetime)" (_childrenBorneLifetime w)
      : iStat "children reared (lifetime)" (_childrenWeanedLifetime w)
      : dStat "adult energy" (uiToDouble e)
      : dStat "child energy" (uiToDouble ec)
      : iStat "genome length" ( (length . fst . _genome $ w)
                                  + (length . snd . _genome $ w) )
      : []
    where e = _energy w
          ec = sum . map (view energy) $ _litter w

instance (Serialize p, Serialize t, Serialize a, Eq a, Ord a,
  Tweaker t, p ~ Pattern t)
    => Serialize (Wain p t a)

-- This implementation is useful for generating the genes in the
-- initial population, and for testing
instance (Genetic p, Genetic t, Genetic a, Eq a, Ord a,
  Serialize p, Serialize t, Serialize a, Tweaker t, p ~ Pattern t)
    => Genetic (Wain p t a) where
  put w = put (_appearance w)
            >> put (_brain w)
            >> put (_devotion w)
            >> put (_ageOfMaturity w)
            >> put (_passionDelta w)
            >> put (_boredomDelta w)
  get = do
    g <- copy
    wAppearance <- get
    wBrain <- get
    wDevotion <- get
    wAgeOfMaturity <- get
    wPassionDelta <- get
    wBoredomDelta <- get
    return $ buildWain "" <$> wAppearance <*> wBrain <*> wDevotion
             <*> wAgeOfMaturity <*> wPassionDelta <*> wBoredomDelta
               <*> pure (g, g)

-- This implementation is useful for testing
instance (Diploid p, Diploid t, Diploid a,
  Genetic p, Genetic t, Genetic a, Eq a, Ord a,
    Serialize p, Serialize t, Serialize a, Tweaker t, p ~ Pattern t)
      => Diploid (Wain p t a) where
  express x y = buildWain "" wAppearance wBrain wDevotion
                  wAgeOfMaturity wPassionDelta wBoredomDelta ([],[])
    where wAppearance     = express (_appearance x)    (_appearance y)
          wBrain          = express (_brain x)         (_brain y)
          wDevotion       = express (_devotion x)      (_devotion y)
          wAgeOfMaturity  = express (_ageOfMaturity x)
                                                      (_ageOfMaturity y)
          wPassionDelta   = express (_passionDelta x)  (_passionDelta y)
          wBoredomDelta   = express (_boredomDelta x)  (_boredomDelta y)

instance Agent (Wain p t a) where
  agentId = view name
  isAlive w = _energy w > 0

instance (Genetic p, Genetic t, Genetic a,
  Diploid p, Diploid t, Diploid a,
    Serialize p, Serialize t, Serialize a,
      Eq a, Ord a, Tweaker t, p ~ Pattern t)
        => Reproductive (Wain p t a) where
  type Strand (Wain p t a) = Sequence
  produceGamete a =
    repeatWithProbability 0.1 randomCrossover (_genome a) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build n = runDiploidReader (buildWainFromGenome False n)

-- | Returns the total energy of all children in the litter.
childEnergy :: Wain p t a -> Double
childEnergy = sum . map (uiToDouble . view energy) . view litter

-- | Returns @True@ if a wain is currently raising children; returns
--   @False@ otherwise.
hasLitter :: Wain p t a -> Bool
hasLitter = not . null . view litter

-- | Returns the number of children that a wain is currently raising.
litterSize :: Wain p t a -> Int
litterSize = length . view litter

-- | Returns @True@ if the wain is mature; returns @False@ otherwise.
mature :: Wain p t a -> Bool
mature a = _age a >= _ageOfMaturity a

-- | Returns the wain's current condition. This is useful for making
--   decisions.
condition :: Wain p t a -> B.Condition
condition w = [ _energy w, 1 - _passion w, 1 - _boredom w,
                if l > 0 then 1 else 0 ]
  where l = length . _litter $ w

-- | Returns the wain's current happiness level.
--   This is a number between 0 and 1, inclusive.
happiness :: Wain p t a -> UIDouble
happiness w = B.happiness (_brain w) (condition w)

--numModels . view B.classifier . view brain $ w

-- | Chooses a response based on the stimuli (input patterns) and
--   the wain's condition.
--   Returns the classifier labels assigned to each input pattern,
--   the classifier signature of each input pattern,
--   the predictor model on which the response is based,
--   the responses it considered (with outcome predictions filled in,
--   and paired with predictor model labels),
--   the chosen response, and the updated wain.
--
--   NOTE: that the response chosen might be a response modelled on
--   a different scenario than the one we think we're in.
--   I.e., @cBMUs@ may not equal @view (scenario . labels) r@.
--   This might happen, for example, if the ideal response to the
--   most likely scenario has a somewhat good outcome, but the ideal
--   response to a somewhat likely alternative scenario has a really
--   bad outcome. "I think that food is edible, but I'm not going to
--   eat it just in case I've misidentified it and it's poisonous."
chooseAction
  :: (Eq a, Enum a, Bounded a, Ord a)
    => [p] -> Wain p t a
      -> ([[(Cl.Label, Cl.Difference)]],
          [([Cl.Label], Probability)],
          [(R.Response a, Probability, P.Label, [PM1Double])],
          [(a, [PM1Double], UIDouble)],
          R.Response a,
          Wain p t a)
chooseAction ps w = (lds, sps, rplos, aohs, r, w')
  where (lds, sps, rplos, aohs, r, b')
          = B.chooseAction (_brain w) ps (condition w)
        w' = set brain b' w

-- -- | @'applyMetabolismCost' baseCost costPerByte childCostFactor w@
-- --   deducts the appropriate metabolism cost from a wain, and any
-- --   children in its litter.
-- --   Returns the update wain (including litter), the energy deducted
-- --   from the wain, and the total energy deducted from the litter.
-- applyMetabolismCost
--   :: Double -> Double -> Double -> Wain p t a
--     -> (Wain p t a, Double, Double)
-- applyMetabolismCost baseCost costPerByte childCostFactor w
--   = (set litter childrenAfter adultAfter, adultCost, childCost)
--   where (adultAfter, adultCost)
--           = applyMetabolismCost1 baseCost costPerByte 1 w
--         xs = map f $ _litter w
--         f = applyMetabolismCost1 baseCost costPerByte childCostFactor
--         childrenAfter = map fst xs
--         childCost = sum . map snd $ xs

-- applyMetabolismCost1
--   :: Double -> Double -> Double -> Wain p t a -> (Wain p t a, Double)
-- applyMetabolismCost1 baseCost costPerByte factor w = (w', delta')
--   where (w', delta', _) = adjustEnergy1 delta w
--         adultCost = baseCost + costPerByte * fromIntegral (_wainSize w)
--         delta = adultCost * factor

-- | Adjusts the energy of a wain.
--   NOTE: A wain's energy is capped to the range [0,1].
adjustEnergy
  :: Double -> Wain p t a -> (Wain p t a, Double)
adjustEnergy delta w = (wAfter, delta')
  where eBefore = _energy w
        eAfter = forceDoubleToUI $ uiToDouble (_energy w) + delta
        wAfter = set energy eAfter w
        delta' = uiToDouble eAfter - uiToDouble eBefore

-- | Adjusts the boredom level of a wain.
--   Note: A wain's boredom is capped to the range [0,1].
adjustBoredom
  :: Double -> Wain p t a -> (Wain p t a, Double)
adjustBoredom delta w = (w', delta')
  where (w', delta', _) = adjustBoredom1 delta w

adjustBoredom1
  :: Double -> Wain p t a -> (Wain p t a, Double, Double)
adjustBoredom1 delta w = (wAfter, delta', leftover)
  where bBefore = _boredom w
        bAfter = forceDoubleToUI $ uiToDouble (_boredom w) + delta
        wAfter = set boredom bAfter w
        delta' = uiToDouble bAfter - uiToDouble bBefore
        leftover = delta - delta'

-- | Adjusts the wain's passion by the genetically-determined amount.
--   NOTE: The passion is capped to the range [0,1]. The litter is not
--   affected.
autoAdjustPassion :: Wain p t a -> Wain p t a
autoAdjustPassion w = set passion p w
  where p = doubleToUI . enforceRange unitInterval $
              uiToDouble (_passion w) + uiToDouble (_passionDelta w)

-- | Adjusts the wain's boredom by the genetically-determined amount.
--   Note: The boredom is capped to the range [0,1]. The litter is not
--   affected.
autoAdjustBoredom :: Wain p t a -> Wain p t a
autoAdjustBoredom w = set boredom p w
  where p = doubleToUI . enforceRange unitInterval $
              uiToDouble (_boredom w) + uiToDouble (_boredomDelta w)

-- | Resets the wain's passion to zero.
--   This would normally be called immediately after mating.
coolPassion :: Wain p t a -> Wain p t a
coolPassion = set passion 0

-- | Increments the age of the wain, and its litter (if any).
incAge :: Wain p t a -> Wain p t a
incAge = incAge1 . incLitterAge

incLitterAge :: Wain p t a -> Wain p t a
incLitterAge w = set litter litter' w
  where litter' = map incAge1 $ _litter w

incAge1 :: Wain p t a -> Wain p t a
incAge1 = age +~ 1

-- | Causes a wain to considers whether it is happier or not as a
--   result of the last action it took, and modifies its decision models
--   accordingly. The wain's litter, if any, will not have access to
--   the parent's internal condition, and their own condition will not
--   change, so they do not have any way to assess whether the outcome
--   of the action was good. Instead they will simply assume that
--   the action was perfect (increased happiness by 1).
--   TODO: Do something more realistic.
reflect
  :: (Serialize p, Serialize t, Serialize a, Eq a, Ord a, Tweaker t,
    p ~ Pattern t)
      => [p] -> R.Response a -> Wain p t a -> Wain p t a
        -> (Wain p t a, Double)
reflect ps r wBefore wAfter =
  (set litter litter' wReflected, err)
  where (wReflected, err) = reflect1 r wBefore wAfter
        a = R._action r
        litter' = map (imprint ps a) (_litter wAfter)

reflect1
  :: Eq a
    => R.Response a -> Wain p t a -> Wain p t a -> (Wain p t a, Double)
reflect1 r wBefore wAfter = (set brain b' wAfter, err)
  where (b', err) = B.reflect (_brain wAfter) r (condition wBefore)
                      (condition wAfter)

-- | Teaches the wain that the last action taken was a perfect one
--   (increased happiness by 1).
--   This can be used to help children learn by observing their parents.
imprint
  :: (Serialize p, Serialize t, Serialize a, Eq a, Ord a, Tweaker t,
    p ~ Pattern t)
      => [p] -> a -> Wain p t a -> Wain p t a
imprint ps a w = set brain b' w
  where b' = B.imprint (_brain w) ps a

-- | Attempts to mate two wains.
--   If either of the wains already has a litter, mating will not occur.
--   If mating does occur, the passion level of both wains will be
--   reset to zero.
--   Returns the (possibly modified) wains, descriptions of any errors
--   that occurred when attempting to produce a child from the genome,
--   and the energy contribution from each parent.
mate
  :: (RandomGen r, Diploid p, Diploid t, Diploid a,
    Genetic p, Genetic t, Genetic a,
      Serialize p, Serialize t, Serialize a,
        Eq a, Ord a, Tweaker t, p ~ Pattern t)
          => Wain p t a -> Wain p t a -> String
            -> Rand r ([Wain p t a], [String], Double, Double)
mate a b babyName
  | hasLitter a
      = return ([a, b], [_name a ++ " already has a litter"], 0, 0)
  | hasLitter b
      = return ([a, b], [_name b ++ " already has a litter"], 0, 0)
  | otherwise = mate' a b babyName

mate'
  :: (RandomGen r, Diploid p, Diploid t, Diploid a,
    Genetic p, Genetic t, Genetic a,
      Serialize p, Serialize t, Serialize a,
        Eq a, Ord a, Tweaker t, p ~ Pattern t)
          => Wain p t a -> Wain p t a -> String
            -> Rand r ([Wain p t a], [String], Double, Double)
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
  :: Wain p t a -> Wain p t a -> Wain p t a
     -> (Wain p t a, Wain p t a, Wain p t a, Double, Double)
donateParentEnergy a b c = (a', b', c', aContribution', bContribution')
  where aContribution = - uiToDouble (_devotion a * _energy a)
        bContribution = - uiToDouble (_devotion b * _energy b)
        (a', aContribution') = adjustEnergy aContribution a
        (b', bContribution') = adjustEnergy bContribution b
        cContribution = -(aContribution' + bContribution')
        (c', _) = adjustEnergy cContribution c

-- | Removes any mature children from the wain's litter.
--   Returns a list containing the (possibly modified) wain, together
--   with any children that have just been weaned.
weanMatureChildren :: Wain p t a -> [Wain p t a]
weanMatureChildren a =
  if null (_litter a)
    then [a]
    else a':weanlings
  where (weanlings, babes) = partition mature (_litter a)
        newWeanlings = fromIntegral $ length weanlings
        a' = (litter .~ babes)
               . (childrenWeanedLifetime +~ newWeanlings) $ a

-- | Removes any dead children from the wain's litter.
pruneDeadChildren :: Wain p t a -> [Wain p t a]
pruneDeadChildren a =
  if null (_litter a)
    then [a]
    else a':deadChildren
  where (aliveChildren, deadChildren) = partition isAlive (_litter a)
        a' = set litter aliveChildren a

