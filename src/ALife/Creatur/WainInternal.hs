------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.WainInternal
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module ALife.Creatur.WainInternal where

import           ALife.Creatur
    (Agent, agentId, isAlive)
import           ALife.Creatur.Database
    (Record, SizedRecord, key)
import qualified ALife.Creatur.Database                     (size)
import           ALife.Creatur.Genetics.BRGCWord8
    (DiploidReader, Genetic, Sequence, consumed2, copy, copy2, get,
    getAndExpress, put, runDiploidReader, write)
import           ALife.Creatur.Genetics.Diploid             (Diploid, express)
import           ALife.Creatur.Genetics.Recombination
    (mutatePairedLists, randomCrossover, randomCutAndSplice, randomOneOfPair,
    repeatWithProbability, withProbability)
import           ALife.Creatur.Genetics.Reproduction.Sexual
    (Reproductive, Strand, build, makeOffspring, produceGamete)
import qualified ALife.Creatur.Wain.Brain                   as B
import qualified ALife.Creatur.Wain.Classifier              as Cl
import qualified ALife.Creatur.Wain.GeneticSOM              as GSOM
import           ALife.Creatur.Wain.Muser                   (Action, Muser)
import qualified ALife.Creatur.Wain.Predictor               as P
import           ALife.Creatur.Wain.Pretty                  (Pretty, pretty)
import           ALife.Creatur.Wain.Report                  (Report, report)
import qualified ALife.Creatur.Wain.Response                as R
import           ALife.Creatur.Wain.Statistics
    (Statistical, dStat, iStat, stats)
import           ALife.Creatur.Wain.UnitInterval
    (UIDouble, doubleToUI, forceDoubleToUI, uiToDouble)
import           ALife.Creatur.Wain.Util
    (enforceRange, unitInterval)
import           Control.DeepSeq                            (NFData)
import           Control.Lens
import           Control.Monad.Random                       (Rand, RandomGen)
import           Data.List
    (intercalate, partition, sortBy)
import qualified Data.Map.Strict                            as M
import           Data.Ord                                   (comparing)
import           Data.Serialize                             (Serialize)
import           Data.Version                               (showVersion)
import           Data.Word                                  (Word16, Word8)
import           GHC.Generics                               (Generic)
import           Paths_creatur_wains                        (version)
import           Text.Printf                     (printf)

-- | Returns the current version number of this library.
packageVersion :: String
packageVersion = "creatur-wains-" ++ showVersion version

-- | A data mining agent that can learn, reproduce, and evolve.
data Wain p ct pt m a = Wain
  {
    -- | Each wain should have a unique name.
    _name                   :: String,
    -- | A wain's appearance is a pattern by which other wains can
    --   recognise it as a fellow wain.
    _appearance             :: p,
    -- | The wain's brain, which recognises patterns and makes
    --   decisions.
    _brain                  :: B.Brain p ct pt m a,
    -- | The amount of energy the wain will give to offspring at birth.
    --   This is a number between 0 and 1, inclusive.
    _devotion               :: UIDouble,
    -- | The age at which this wain will/has left its parent.
    _ageOfMaturity          :: Word16,
    -- | The amount that a wain's passion increases at each CPU turn.
    --   this influences the frequency of mating.
    _passionDelta           :: UIDouble,
    -- | The amount that a wain's boredom increases at each CPU turn.
    --   this influences the frequency of mating.
    _boredomDelta           :: UIDouble,
    -- | The wain's current energy level.
    --   This is a number between 0 and 1, inclusive.
    _energy                 :: UIDouble,
    -- | The wain's current passion level
    --   This is a number between 0 and 1, inclusive.
    _passion                :: UIDouble,
    -- | The wain's current boredom level
    --   This is a number between 0 and 1, inclusive.
    _boredom                :: UIDouble,
    -- | The wain's current age.
    _age                    :: Word16,
    -- | The children this wain is currently rearing.
    _litter                 :: [Wain p ct pt m a],
    -- | The number of children this wain has borne.
    _childrenBorneLifetime  :: Word16,
    -- | The number of children this wain has reared to maturity.
    _childrenWeanedLifetime :: Word16,
    -- | The wain's genes.
    _genome                 :: ([Word8],[Word8])
  } deriving (Eq, Generic, NFData)
makeLenses ''Wain

-- | Internal method
buildWain
  :: (Genetic p, Genetic ct, Genetic pt, Genetic a, Eq a,
    GSOM.Tweaker ct, GSOM.Tweaker pt, p ~ GSOM.Pattern ct,
    R.Response a ~ GSOM.Pattern pt,
    Serialize p, Serialize ct, Serialize pt, Serialize a, Ord a)
  => String -> p -> B.Brain p ct pt m a -> UIDouble -> Word16
  -> UIDouble -> UIDouble -> (Sequence, Sequence) -> Wain p ct pt m a
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

-- | Constructs a wain with the specified parameters, with the
--   corresponding (generated) genome. This would normally be used only
--   for generating the initial population.
buildWainAndGenerateGenome
  :: (Genetic p, Genetic ct, Genetic pt, Genetic m, Genetic a,
    Serialize p, Serialize ct, Serialize pt, Serialize a,
    Eq a, Ord a, GSOM.Tweaker ct, GSOM.Tweaker pt, p ~ GSOM.Pattern ct,
    R.Response a ~ GSOM.Pattern pt, Muser m)
  => String -> p -> B.Brain p ct pt m a -> UIDouble -> Word16
  -> UIDouble -> UIDouble -> Wain p ct pt m a
buildWainAndGenerateGenome wName wAppearance wBrain wDevotion
  wAgeOfMaturity wPassionDelta wBoredomDelta = set genome (g,g) strawMan
  where strawMan = buildWain wName wAppearance wBrain wDevotion
                     wAgeOfMaturity wPassionDelta wBoredomDelta ([], [])
        g = write strawMan

-- | Constructs a wain from its genome. This is used when a child is
--   produced as the result of mating.
buildWainFromGenome
  :: (Genetic p, Genetic ct, Genetic pt, Genetic m, Genetic a,
    Diploid p, Diploid ct, Diploid pt, Diploid m, Diploid a,
    Serialize p, Serialize ct, Serialize pt, Serialize a,
    Ord a, Eq a, GSOM.Tweaker ct, GSOM.Tweaker pt,
    p ~ GSOM.Pattern ct, R.Response a ~ GSOM.Pattern pt, Muser m)
  => Bool -> String
  -> DiploidReader (Either [String] (Wain p ct pt m a))
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

deriving instance (Show p, Show ct, Show pt, Show m, Show a, Eq a)
    => Show (Wain p ct pt m a)

instance Record (Wain p ct pt m a) where
  key = view name

instance SizedRecord (Wain p ct pt m a) where
  size = const 1

instance (Eq a, Ord a,
          Statistical [(GSOM.Label, p)], Statistical ct,
          Statistical pt, Statistical m,
          Statistical [(GSOM.Label, R.Response a)])
    => Statistical (Wain p ct pt m a) where
  stats w =
    iStat "age" (_age w)
      : stats (_brain w)
      ++ dStat "devotion" (_devotion w)
      : iStat "maturity" (_ageOfMaturity w)
      : dStat "Δp" (_passionDelta w)
      : dStat "Δb" (_boredomDelta w)
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

instance (Serialize p, Serialize ct, Serialize pt, Serialize m,
  Serialize a, Eq a, Ord a, GSOM.Tweaker ct, GSOM.Tweaker pt,
  p ~ GSOM.Pattern ct, R.Response a ~ GSOM.Pattern pt)
  => Serialize (Wain p ct pt m a)

-- This implementation is useful for generating the genes in the
-- initial population, and for testing
instance (Genetic p, Genetic ct, Genetic pt, Genetic m, Genetic a,
  Eq a, Ord a, Serialize p, Serialize ct, Serialize pt, Serialize a,
  GSOM.Tweaker ct, GSOM.Tweaker pt, p ~ GSOM.Pattern ct,
  R.Response a ~ GSOM.Pattern pt, Muser m)
  => Genetic (Wain p ct pt m a) where
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
instance (Diploid p, Diploid ct, Diploid pt, Diploid m, Diploid a,
  Genetic p, Genetic ct, Genetic pt, Genetic m, Genetic a, Eq a, Ord a,
    Serialize p, Serialize ct, Serialize pt, Serialize a, GSOM.Tweaker ct,
      GSOM.Tweaker pt, p ~ GSOM.Pattern ct, R.Response a ~ GSOM.Pattern pt)
        => Diploid (Wain p ct pt m a) where
  express x y = buildWain "" wAppearance wBrain wDevotion
                  wAgeOfMaturity wPassionDelta wBoredomDelta ([],[])
    where wAppearance     = express (_appearance x)    (_appearance y)
          wBrain          = express (_brain x)         (_brain y)
          wDevotion       = express (_devotion x)      (_devotion y)
          wAgeOfMaturity  = express (_ageOfMaturity x)
                                                      (_ageOfMaturity y)
          wPassionDelta   = express (_passionDelta x)  (_passionDelta y)
          wBoredomDelta   = express (_boredomDelta x)  (_boredomDelta y)

instance Agent (Wain p ct pt m a) where
  agentId = view name
  isAlive w = _energy w > 0

instance (Genetic p, Genetic ct, Genetic pt, Genetic m, Genetic a,
  Diploid p, Diploid ct, Diploid pt, Diploid m, Diploid a,
  Serialize p, Serialize ct, Serialize pt, Serialize a,
  Eq a, Ord a, GSOM.Tweaker ct, GSOM.Tweaker pt, p ~ GSOM.Pattern ct,
  R.Response a ~ GSOM.Pattern pt, Muser m)
  => Reproductive (Wain p ct pt m a) where
  type Strand (Wain p ct pt m a) = Sequence
  produceGamete a =
    repeatWithProbability 0.1 randomCrossover (_genome a) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build n = runDiploidReader (buildWainFromGenome False n)

-- | Returns the total energy of all children in the litter.
childEnergy :: Wain p ct pt m a -> Double
childEnergy = sum . map (uiToDouble . view energy) . view litter

-- | Returns @True@ if a wain is currently raising children; returns
--   @False@ otherwise.
hasLitter :: Wain p ct pt m a -> Bool
hasLitter = not . null . view litter

-- | Returns the number of children that a wain is currently raising.
litterSize :: Wain p ct pt m a -> Int
litterSize = length . view litter

-- | Returns @True@ if the wain is mature; returns @False@ otherwise.
mature :: Wain p ct pt m a -> Bool
mature a = _age a >= _ageOfMaturity a

-- | Returns the wain's current condition. This is useful for making
--   decisions.
condition :: Wain p ct pt m a -> B.Condition
condition w = [ _energy w, 1 - _passion w, 1 - _boredom w,
                if l > 0 then 1 else 0 ]
  where l = length . _litter $ w

-- | Returns the wain's current happiness level.
--   This is a number between 0 and 1, inclusive.
happiness :: Wain p ct pt m a -> UIDouble
happiness w = B.happiness (_brain w) (condition w)

--numModels . view B.classifier . view brain $ w

-- | Detailed information about how a decision was made.
data DecisionReport p a =
  DecisionReport
    {
      wdrStimulus         :: [p],
      wdrClassifierReport :: Cl.ClassifierReport p,
      wdrScenarioReport   :: B.ScenarioReport,
      wdrPredictorReport  :: B.PredictorReport a,
      wdrActionReport     :: B.ActionReport a,
      wdrImprintReports   :: [StimulusImprintReport p]
    } deriving (Generic, Show, NFData)

-- | Returns the measure of how novel each input pattern was to the
--   wain.
novelties :: DecisionReport p a -> [UIDouble]
novelties = map GSOM.cNovelty . Cl.cDetails . wdrClassifierReport

-- | Returns the measure of how novel each input pattern was to the
--   wain, adjusted for the age of the wain.
adjNovelties :: DecisionReport p a -> [Int]
adjNovelties = map GSOM.cAdjNovelty . Cl.cDetails . wdrClassifierReport

-- | Chooses a response based on the stimuli (input patterns) and
--   the wain's condition.
--   Returns a detailed report of the decision process
--   and the updated wain.
--
--   NOTE: The response chosen might be a response modelled on
--   a different scenario than the one we think we're in.
--   This might happen, for example, if the ideal response to the
--   most likely scenario has a somewhat good outcome, but the ideal
--   response to a somewhat likely alternative scenario has a really
--   bad outcome. "I think that food is edible, but I'm not going to
--   eat it just in case I've misidentified it and it's poisonous."
chooseAction
  :: (Muser m, Action m ~ a, Eq a, Ord a, GSOM.Tweaker pt,
     R.Response a ~ GSOM.Pattern pt)
      => [p] -> Wain p ct pt m a
        -> (DecisionReport p a, R.Response a, Wain p ct pt m a)
chooseAction ps w = (dReport', r, set litter litter' w')
  where (dReport, b')
          = B.chooseAction (_brain w) ps (condition w)
        r = B.bdrRecommendedResponse dReport
        w' = set brain b' w
        ls = R._labels r
        lps = zip ls ps
        iResults = map (imprintStimulus lps) (_litter w')
        litter' = map snd iResults
        dReport' = DecisionReport
                    {
                      wdrStimulus = B.bdrStimulus dReport,
                      wdrClassifierReport
                        = B.bdrClassifierReport dReport,
                      wdrScenarioReport = B.bdrScenarioReport dReport,
                      wdrPredictorReport = B.bdrPredictorReport dReport,
                      wdrActionReport = B.bdrActionReport dReport,
                      wdrImprintReports = map fst iResults
                    }

-- | Adjusts the energy of a wain.
--   NOTE: A wain's energy is capped to the range [0,1].
adjustEnergy
  :: Double -> Wain p ct pt m a -> (Wain p ct pt m a, Double)
adjustEnergy delta w = (wAfter, delta')
  where eBefore = _energy w
        eAfter = forceDoubleToUI $ uiToDouble (_energy w) + delta
        wAfter = set energy eAfter w
        delta' = uiToDouble eAfter - uiToDouble eBefore

-- | Adjusts the boredom level of a wain.
--   Note: A wain's boredom is capped to the range [0,1].
adjustBoredom
  :: Double -> Wain p ct pt m a -> (Wain p ct pt m a, Double)
adjustBoredom delta w = (w', delta')
  where (w', delta', _) = adjustBoredom1 delta w

-- | Internal method
adjustBoredom1
  :: Double -> Wain p ct pt m a -> (Wain p ct pt m a, Double, Double)
adjustBoredom1 delta w = (wAfter, delta', leftover)
  where bBefore = _boredom w
        bAfter = forceDoubleToUI $ uiToDouble (_boredom w) + delta
        wAfter = set boredom bAfter w
        delta' = uiToDouble bAfter - uiToDouble bBefore
        leftover = delta - delta'

-- | Adjusts the wain's passion by the genetically-determined amount.
--   NOTE: The passion is capped to the range [0,1]. The litter is not
--   affected.
autoAdjustPassion :: Wain p ct pt m a -> Wain p ct pt m a
autoAdjustPassion w = set passion p w
  where p = doubleToUI . enforceRange unitInterval $
              uiToDouble (_passion w) + uiToDouble (_passionDelta w)

-- | Adjusts the wain's boredom by the genetically-determined amount.
--   Note: The boredom is capped to the range [0,1]. The litter is not
--   affected.
autoAdjustBoredom :: Wain p ct pt m a -> Wain p ct pt m a
autoAdjustBoredom w = set boredom p w
  where p = doubleToUI . enforceRange unitInterval $
              uiToDouble (_boredom w) + uiToDouble (_boredomDelta w)

-- | Resets the wain's passion to zero.
--   This would normally be called immediately after mating.
coolPassion :: Wain p ct pt m a -> Wain p ct pt m a
coolPassion = set passion 0

-- | Increments the age of the wain, and its litter (if any).
incAge :: Wain p ct pt m a -> Wain p ct pt m a
incAge = incAge1 . incLitterAge

-- | Internal method
incLitterAge :: Wain p ct pt m a -> Wain p ct pt m a
incLitterAge w = set litter litter' w
  where litter' = map incAge1 $ _litter w

-- | Internal method
incAge1 :: Wain p ct pt m a -> Wain p ct pt m a
incAge1 = age +~ 1

data ReflectionReport p a
  = ReflectionReport
      {
        rReflectionReport :: B.ReflectionReport a,
        rImprintReports   :: [P.LearningReport a]
      } deriving (Generic, Show, NFData)

happinessError :: ReflectionReport p a -> Double
happinessError = B.brrErr . rReflectionReport

-- | Causes a wain to considers whether it is happier or not as a
--   result of the last action it took, and modifies its decision models
--   accordingly. The wain's litter, if any, will not have access to
--   the parent's internal condition, and their own condition will not
--   change, so they do not have any way to assess whether the outcome
--   of the action was good. Instead they will simply assume that
--   the action was perfect (increased happiness by 1).
--   TODO: Do something more realistic.
reflect
  :: (Serialize p, Serialize ct, Serialize pt, Serialize a, Eq a, Ord a,
    GSOM.Tweaker ct, GSOM.Tweaker pt, p ~ GSOM.Pattern ct,
    R.Response a ~ GSOM.Pattern pt)
  => R.Response a -> Wain p ct pt m a -> Wain p ct pt m a
  -> (ReflectionReport p a, Wain p ct pt m a)
reflect r wBefore wAfter =
  (rReport', set litter litter' wReflected)
  where (rReport, wReflected) = reflect1 r wBefore wAfter
        a = R._action r
        ls = R._labels r
        iResults = map (imprintResponse ls a) (_litter wAfter)
        litter' = map snd iResults
        rReport' = ReflectionReport
                   {
                     rReflectionReport = rReport,
                     rImprintReports = map fst iResults
                   }

-- | Internal method
reflect1
  :: Eq a
  => R.Response a -> Wain p ct pt m a -> Wain p ct pt m a
  -> (B.ReflectionReport a, Wain p ct pt m a)
reflect1 r wBefore wAfter = (rReport, set brain b' wAfter)
  where (rReport, b') = B.reflect (_brain wAfter) r (condition wBefore)
                        (condition wAfter)

type StimulusImprintReport p = [GSOM.ImprintDetail p]

-- | Teaches the wain a set of patterns, and a label for each of them.
imprintStimulus
  :: [(Cl.Label, p)]
  -> Wain p ct pt m a
  -> (StimulusImprintReport p, Wain p ct pt m a)
imprintStimulus lps w = (iReport, set brain b' w)
  where (iReport, b') = B.imprintStimulus (_brain w) lps

type ResponseImprintReport a = P.LearningReport a

-- | Teaches the wain a desirable action to take in response to a
--   stimulus.
--   This can be used to help children learn by observing their parents.
--   It can also be used to allow wains to learn from others.
--   Returns a detailed report of the imprint process
--   and the updated wain.
imprintResponse
  :: (Serialize p, Serialize ct, Serialize pt, Serialize a, Eq a, Ord a,
    GSOM.Tweaker ct, GSOM.Tweaker pt, p ~ GSOM.Pattern ct,
    R.Response a ~ GSOM.Pattern pt)
  => [Cl.Label] -> a -> Wain p ct pt m a
  -> (P.LearningReport a, Wain p ct pt m a )
imprintResponse ls a w = (iReport, set brain b' w)
  where (iReport, b') = B.imprintResponse (_brain w) ls a

-- | Attempts to mate two wains.
--   If either of the wains already has a litter, mating will not occur.
--   If mating does occur, the passion level of both wains will be
--   reset to zero.
--   Returns the (possibly modified) wains, descriptions of any errors
--   that occurred when attempting to produce a child from the genome,
--   and the energy contribution from each parent.
mate
  :: (RandomGen r, Diploid p, Diploid ct, Diploid pt, Diploid m,
    Diploid a, Genetic p, Genetic ct, Genetic pt, Genetic m, Genetic a,
    Serialize p, Serialize ct, Serialize pt, Serialize a,
    Eq a, Ord a, GSOM.Tweaker ct, GSOM.Tweaker pt, p ~ GSOM.Pattern ct,
    R.Response a ~ GSOM.Pattern pt, Muser m)
  => Wain p ct pt m a -> Wain p ct pt m a -> String
  -> Rand r ([Wain p ct pt m a], [String], Double, Double)
mate a b babyName
  | hasLitter a
      = return ([a, b], [_name a ++ " already has a litter"], 0, 0)
  | hasLitter b
      = return ([a, b], [_name b ++ " already has a litter"], 0, 0)
  | otherwise = mate' a b babyName

-- | Internal method
mate'
  :: (RandomGen r, Diploid p, Diploid ct, Diploid pt, Diploid m,
    Diploid a, Genetic p, Genetic ct, Genetic pt, Genetic m, Genetic a,
    Serialize p, Serialize ct, Serialize pt, Serialize a,
    Eq a, Ord a, GSOM.Tweaker ct, GSOM.Tweaker pt, p ~ GSOM.Pattern ct,
    R.Response a ~ GSOM.Pattern pt, Muser m)
   => Wain p ct pt m a -> Wain p ct pt m a -> String
   -> Rand r ([Wain p ct pt m a], [String], Double, Double)
mate' a b babyName = do
  let a2 = coolPassion a
  let b2 = coolPassion b
  result <- makeOffspring a b babyName
  case result of
    Right baby -> do
      let (a3, b3, baby3, aContribution, bContribution)
             = donateParentEnergy a2 b2 baby
      let a4 = (litter .~ [baby3])
                 . (childrenBorneLifetime +~ 1) $ a3
      return ([a4, b3], [], aContribution, bContribution)
    Left msgs -> return ([a2, b2], msgs, 0, 0)

-- | Internal method
donateParentEnergy
  :: Wain p ct pt m a -> Wain p ct pt m a -> Wain p ct pt m a
     -> (Wain p ct pt m a, Wain p ct pt m a, Wain p ct pt m a,
          Double, Double)
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
weanMatureChildren :: Wain p ct pt m a -> [Wain p ct pt m a]
weanMatureChildren a =
  if null (_litter a)
    then [a]
    else a':weanlings
  where (weanlings, babes) = partition mature (_litter a)
        newWeanlings = fromIntegral $ length weanlings
        a' = (litter .~ babes)
               . (childrenWeanedLifetime +~ newWeanlings) $ a

-- | Removes any dead children from the wain's litter.
pruneDeadChildren :: Wain p ct pt m a -> [Wain p ct pt m a]
pruneDeadChildren a =
  if null (_litter a)
    then [a]
    else a':deadChildren
  where (aliveChildren, deadChildren) = partition isAlive (_litter a)
        a' = set litter aliveChildren a

-- | Describes the classifier models.
prettyClassifierModels :: Pretty p => Wain p ct pt m a -> [String]
prettyClassifierModels w
  = prettyClassifierModels' w
      ++ concatMap (indent . prettyClassifierModels') (_litter w)

prettyClassifierModels' :: Pretty p => Wain p ct pt m a -> [String]
prettyClassifierModels' w
  = (agentId w ++ "'s classifier models") : map f ms
  where ms = M.toList . GSOM.modelMap . view (brain . B.classifier) $ w
        f (l, m) = show l ++ " " ++ pretty m

-- | Describes the predictor models.
prettyPredictorModels :: Pretty a => Wain p ct pt m a -> [String]
prettyPredictorModels w
  = prettyPredictorModels' w
      ++ concatMap (indent . prettyPredictorModels') (_litter w)

prettyPredictorModels' :: Pretty a => Wain p ct pt m a -> [String]
prettyPredictorModels' w
  = (agentId w ++ "'s predictor models") : map f ms
  where ms = M.toList . GSOM.modelMap . view (brain . B.predictor) $ w
        f (l, m) = show l ++ ": " ++ pretty m

-- | Generates a report that shows relationships between the classifier
--   and predictor models.
--   Useful when the classifier models and the predictor models are
--   both brief.
prettyBrainSummary :: (Pretty p, Pretty a) => Wain p ct pt m a -> [String]
prettyBrainSummary w
  = (agentId w ++ "'s brain summary") : map f rs
  where ps = M.toList . GSOM.modelMap
               . view (brain . B.classifier) $ w
        rs = sortBy (comparing (R._labels . snd)) . M.toList
               . GSOM.modelMap . view (brain . B.predictor) $ w
        f (l, r) = show l ++ ": "
                     ++ intercalate "," (map g (R._labels r)) ++ " "
                     ++ pretty r
        g l' = show l' ++ " " ++ pretty (lookup l' ps)

prettyClassificationReport
  :: Pretty p
  => Wain p ct pt m a -> DecisionReport p a -> [String]
prettyClassificationReport w r
  = (agentId w ++ " classifies the input(s)")
    : Cl.prettyClassifierReport (wdrClassifierReport r)
    ++ concatMap f wrs
  where wrs = zip (_litter w) (wdrImprintReports r)
        f (child, iReport) = indent $
                               prettyStimulusImprintReport child iReport

prettyScenarioReport
  :: Wain p ct pt m a -> DecisionReport p a -> [String]
prettyScenarioReport w r
  = (agentId w ++ " develops hypotheses about the current scenario")
    : B.prettyScenarioReport (wdrScenarioReport r)

prettyPredictionReport
  :: Pretty a
  => Wain p ct pt m a -> DecisionReport p a -> [String]
prettyPredictionReport w r
  = (agentId w ++ " considers possible responses")
    : map pretty (wdrPredictorReport r)

prettyActionReport
  :: Pretty a
  => Wain p ct pt m a -> DecisionReport p a -> [String]
prettyActionReport w r
  = (agentId w ++ " predicts outcomes of possible responses")
    : B.prettyActionReport (wdrActionReport r)

prettyReflectionReport
  :: (Pretty p, Pretty a)
  => Wain p ct pt m a -> ReflectionReport p a -> [String]
prettyReflectionReport w r
  = (agentId w ++ " reflects on the outcome")
    : B.prettyReflectionReport (rReflectionReport r)
    ++ concatMap f wrs
  where wrs = zip (_litter w) (rImprintReports r)
        f (child, iReport) = indent $
                               prettyResponseImprintReport child iReport

prettyStimulusImprintReport
  :: Pretty p
  => Wain p ct pt m a -> StimulusImprintReport p -> [String]
prettyStimulusImprintReport w r
  = (agentId w ++ " imprints a stimulus")
    : concatMap GSOM.prettyImprintDetail r

prettyResponseImprintReport
  :: Pretty a
  => Wain p ct pt m a -> ResponseImprintReport a -> [String]
prettyResponseImprintReport w r
  = (agentId w ++ " imprints a response")
    : P.prettyLearningReport r

indent :: [String] -> [String]
indent = map ("...." ++ )

instance (Pretty p, Pretty ct, Pretty pt, Pretty m, Pretty a)
    => Report (Wain p ct pt m a) where
  report w = report' w
             ++ map ("child " ++) childReport
    where childReport = concatMap report' (_litter w)

report'
  :: (Pretty p, Pretty ct, Pretty pt, Pretty m, Pretty a)
  => Wain p ct pt m a -> [String]
report' w = map (\s -> n ++ ' ':s) xs
  where n = _name w
        xs = [ "appearance: " ++ pretty (_appearance w),
               "devotion: " ++ printf "%5.3f" (uiToDouble $ _devotion w),
               "ageOfMaturity: " ++ printf "%d" (_ageOfMaturity w),
               "passionDelta: " ++ pretty (_passionDelta w),
               "boredomDelta: " ++ pretty (_boredomDelta w),
               "energy: " ++ printf "%5.3f" (uiToDouble $ _energy w),
               "passion: " ++ printf "%5.3f" (uiToDouble $ _passion w),
               "boredom: " ++ printf "%5.3f" (uiToDouble $ _boredom w),
               "age: " ++ pretty (_age w),
               "total # children borne: "
                 ++ pretty (_childrenBorneLifetime w),
               "total # children weaned: "
                 ++ pretty (_childrenWeanedLifetime w),
               "children: " ++ childrensNames ]
             ++ report (_brain w)
        childrensNames = intercalate ", " $ map _name (_litter w)

