------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.WainInternal
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module ALife.Creatur.WainInternal where

import ALife.Creatur                              (Agent, agentId, isAlive)
import ALife.Creatur.Database                     (Record, SizedRecord, key)
import ALife.Creatur.Database                     qualified (size)
import ALife.Creatur.Gene.Numeric.UnitInterval    qualified as UI
import ALife.Creatur.Gene.Numeric.Util            (enforceRange, unitInterval)
import ALife.Creatur.Genetics.BRGCWord8           (DiploidReader, Genetic,
                                                   Sequence, consumed2, copy,
                                                   copy2, get, getAndExpress,
                                                   put, runDiploidReader, write)
import ALife.Creatur.Genetics.Diploid             (Diploid, express)
import ALife.Creatur.Genetics.Recombination       (mutatePairedLists,
                                                   randomCrossover,
                                                   randomCutAndSplice,
                                                   randomOneOfPair,
                                                   repeatWithProbability,
                                                   withProbability)
import ALife.Creatur.Genetics.Reproduction.Sexual qualified as RS
import ALife.Creatur.Wain.Brain                   qualified as B
import ALife.Creatur.Wain.Classifier              qualified as Cl
import ALife.Creatur.Wain.GeneticSOM              qualified as GSOM
import ALife.Creatur.Wain.Muser                   (Action, Muser)
import ALife.Creatur.Wain.Predictor               qualified as P
import ALife.Creatur.Wain.Pretty                  (Pretty, pretty)
import ALife.Creatur.Wain.Report                  (Report, report)
import ALife.Creatur.Wain.Response                qualified as R
import ALife.Creatur.Wain.Statistics              (Statistical, dStat, iStat,
                                                   stats)
import Control.DeepSeq                            (NFData)
import Control.Monad.Random                       (Rand, RandomGen)
import Data.Datamining.Clustering.SGM4            qualified as SOM
import Data.List                                  (foldl', intercalate, sortOn)
import Data.Map.Strict                            qualified as M
import Data.Serialize                             (Serialize)
import Data.Word                                  (Word16, Word32, Word8)
import GHC.Generics                               (Generic)
import Text.Printf                                (printf)

data Event ct pt p a m
  = IncAge
  | AdjustEnergy Double String
  | AdjustSelfEnergy Double String
  | AdjustLitterEnergy Double String
  | AutoAdjustPassion
  | CoolPassion
  -- | ImprintStimulus [(GSOM.Label, p)]
  | ImprintResponse [p] a
  | Reflect
  | ChooseAction [p]
  | AskToMate
  | AgreeToMate
  | AddChild (Wain ct pt p a m)
  | RemoveChild (Wain ct pt p a m) String
  | Mature
  | Death String
  deriving (Generic, NFData, Read, Show, Eq, Serialize)

runEvent
  :: (SOM.Adjuster ct, Show ct, SOM.PatternType ct ~ p,
     SOM.MetricType ct ~ UI.Double, SOM.TimeType ct ~ Word32,
     SOM.Adjuster pt, Show pt, SOM.PatternType pt ~ R.Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32,
     Pretty p, Show p, Pretty a, Show a, Eq a, Ord a,
     Muser m, Pretty m, Show m, Action m ~ a)
  => Event ct pt p a m -> Wain ct pt p a m -> (Wain ct pt p a m, [String])
runEvent IncAge w                       = incAge w
runEvent AutoAdjustPassion w            = autoAdjustPassion w
runEvent CoolPassion w                  = coolPassion w
runEvent (AdjustSelfEnergy delta source) w  = adjustSelfEnergy delta source w
runEvent (AdjustLitterEnergy delta source) w  = adjustLitterEnergy delta source w
-- runEvent (ImprintStimulus lps) w      = imprintStimulus lps w
runEvent (ImprintResponse ps a) w       = imprintResponse ps a w
runEvent Reflect w                      = reflect w
runEvent (ChooseAction ps) w            = (w', msgs)
  where (w', _, msgs) = chooseAction ps w
runEvent (AddChild c) w                 = addChild c w
runEvent (RemoveChild c source) w       = removeChild c source w
runEvent Mature w                       = markMature w
runEvent (Death source) w               = recordDeath source w
runEvent e w                            = runInfoOnlyEvent e w

recordEvent :: Event ct pt p a m -> Wain ct pt p a m -> Wain ct pt p a m
recordEvent e w = w { biography=e:biography w }

runInfoOnlyEvent
  :: (Show ct, Show pt, Show p, Show a, Show m)
  => Event ct pt p a m -> Wain ct pt p a m -> (Wain ct pt p a m, [String])
runInfoOnlyEvent e w = (recordEvent e w, [show e])

runLife
  :: (SOM.Adjuster ct, Diploid ct, Genetic ct, Show ct,
     SOM.PatternType ct ~ p, SOM.MetricType ct ~ UI.Double,
     SOM.TimeType ct ~ Word32,
     SOM.Adjuster pt, Diploid pt, Genetic pt, Show pt,
     SOM.PatternType pt ~ R.Response a, SOM.MetricType pt ~ UI.Double,
     SOM.TimeType pt ~ Word32,
     Pretty p, Show p, Diploid p, Genetic p,
     Diploid a, Eq a, Genetic a, Ord a, Pretty a, Show a,
     Muser m, Diploid m, Genetic m, Pretty m, Show m, Action m ~ a)
  => Wain ct pt p a m -> (Wain ct pt p a m, [String])
runLife w = foldr runEvent' (w', []) events
  where w' = RS.clone w (agentId w)
        events = biography w

-- | Internal method
runEvent'
  :: (SOM.Adjuster ct, Show ct, SOM.PatternType ct ~ p,
     SOM.MetricType ct ~ UI.Double, SOM.TimeType ct ~ Word32,
     SOM.Adjuster pt, Show pt, SOM.PatternType pt ~ R.Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32,
     Pretty p, Show p, Pretty a, Show a, Eq a, Ord a,
     Muser m, Pretty m, Show m, Action m ~ a)
  => Event ct pt p a m -> (Wain ct pt p a m, [String])
    -> (Wain ct pt p a m, [String])
runEvent' e (w, msgs) = (w', msgs ++ msg:msgs')
  where (w', msgs') = runEvent e w
        msg = "EVENT: " ++ show e

-- | A data mining agent that can learn, reproduce, and evolve.
--   @ct@ is the type of the adjuster used by the classifier,
--   which is a component of the brain.
--   @pt@ is the type of the adjuster used by the predictor.
--   (another brain component)
--   @p@ is the type of the input patterns that the wain "sees",
--   and the mental models that it builds to represent its experiences
--   and make decisions.
--   @a@ is the type of the actions that a wain can take.
--   @m@ is the type of the muser (another brain component).
data Wain ct pt p a m = Wain
  {
    -- | Each wain should have a unique name.
    name          :: String,
    -- | A wain's appearance is a pattern by which other wains can
    --   recognise it as a fellow wain.
    appearance    :: p,
    -- | The wain's brain, which recognises patterns and makes
    --   decisions.
    brain         :: B.Brain ct pt p a m,
    -- | Controls how a wain shares energy rewards with any children
    --   it is currently caring for.
    --   This is a fraction between 0 and 1, inclusive.
    devotion      :: UI.Double,
    -- | The age at which this wain will/has left its parent.
    ageOfMaturity :: Word16,
    -- | The amount that a wain's passion increases at each CPU turn.
    --   this influences the frequency of mating.
    passionDelta  :: UI.Double,
    -- | The wain's current energy level.
    --   This is a number between 0 and 1, inclusive.
    energy        :: UI.Double,
    -- | The wain's current passion level
    --   This is a number between 0 and 1, inclusive.
    passion       :: UI.Double,
    -- | The wain's current age.
    age           :: Word16,
    -- | The children this wain is currently rearing.
    litter        :: [Wain ct pt p a m],
    -- | The wain's genes.
    genome        :: ([Word8],[Word8]),
    -- | The events that occurred during the wain's life.
    biography     :: [Event ct pt p a m]
  } deriving (Eq, Generic, NFData, Show, Read)

-- | Internal method
buildWain
  :: String -> p -> B.Brain ct pt p a m -> UI.Double -> Word16
  -> UI.Double -> (Sequence, Sequence) -> Wain ct pt p a m
buildWain wName wAppearance wBrain wDevotion wAgeOfMaturity
  wPassionDelta g =
    Wain
      {
        name = wName,
        appearance = wAppearance,
        brain = wBrain,
        devotion = wDevotion,
        ageOfMaturity = wAgeOfMaturity,
        passionDelta = wPassionDelta,
        energy = 0,
        passion = 1,
        age = 0,
        litter = [],
        genome = g,
        biography  = []
      }

-- | Constructs a wain with the specified parameters, with the
--   corresponding (generated) genome. This would normally be used only
--   for generating the initial population.
buildWainAndGenerateGenome
  :: (Genetic ct, Genetic pt, Genetic p, Genetic a, Genetic m, Muser m)
  => String -> p -> B.Brain ct pt p a m -> UI.Double -> Word16
  -> UI.Double -> Wain ct pt p a m
buildWainAndGenerateGenome wName wAppearance wBrain wDevotion
  wAgeOfMaturity wPassionDelta = strawMan { genome=(g,g) }
  where strawMan = buildWain wName wAppearance wBrain wDevotion
                     wAgeOfMaturity wPassionDelta ([], [])
        g = write strawMan

-- | Constructs a wain from its genome. This is used when a child is
--   produced as the result of mating.
buildWainFromGenome
  :: (Genetic ct, Diploid ct, Genetic pt, Diploid pt, Diploid p, Genetic p,
     Diploid a, Genetic a, Ord a, Diploid m, Genetic m, Muser m)
  => Bool -> String -> DiploidReader (Either [String] (Wain ct pt p a m))
buildWainFromGenome truncateGenome wName = do
  wAppearance <- getAndExpress
  wBrain <- getAndExpress
  wDevotion <- getAndExpress
  wAgeOfMaturity <- getAndExpress
  wPassionDelta <- getAndExpress
  g <- if truncateGenome then consumed2 else copy2
  return $ buildWain wName <$> wAppearance <*> wBrain <*> wDevotion
             <*> wAgeOfMaturity <*> wPassionDelta <*> pure g

instance Record (Wain ct pt p a m) where
  key = name

instance SizedRecord (Wain ct pt p a m) where
  size = const 1

instance (SOM.Adjuster ct, Statistical ct,
          SOM.Adjuster pt, Statistical pt, Eq a, Statistical m)
  => Statistical (Wain ct pt p a m) where
  stats w =
    iStat "age" (age w)
      : stats (brain w)
      ++ [dStat "devotion" (devotion w),
          iStat "maturity" (ageOfMaturity w),
          dStat "Δp" (passionDelta w),
          dStat "energy" (UI.wide e + UI.wide ec),
          dStat "passion" (passion w),
          dStat "happiness" (happiness w),
          iStat "current litter size" (length . litter $ w),
          dStat "adult energy" (UI.wide e),
          dStat "child energy" (UI.wide ec),
          iStat "genome length" ( (length . fst . genome $ w)
                                  + (length . snd . genome $ w) )]
    where e = energy w
          ec = sum . map energy $ litter w

instance (Serialize ct, Serialize pt, Serialize p, Ord a, Serialize a, Serialize m)
  => Serialize (Wain ct pt p a m)

-- This implementation is useful for generating the genes in the
-- initial population, and for testing
instance (Genetic ct, Genetic pt, Genetic p, Genetic a, Genetic m, Muser m)
  => Genetic (Wain ct pt p a m) where
  put w = put (appearance w)
            >> put (brain w)
            >> put (devotion w)
            >> put (ageOfMaturity w)
            >> put (passionDelta w)
  get = do
    g <- copy
    wAppearance <- get
    wBrain <- get
    wDevotion <- get
    wAgeOfMaturity <- get
    wPassionDelta <- get
    return $ buildWain "" <$> wAppearance <*> wBrain <*> wDevotion
             <*> wAgeOfMaturity <*> wPassionDelta <*> pure (g, g)

-- This implementation is useful for testing
instance (Diploid ct, Diploid pt, Diploid p, Diploid a, Ord a, Diploid m)
  => Diploid (Wain ct pt p a m) where
  express x y = buildWain "" wAppearance wBrain wDevotion
                  wAgeOfMaturity wPassionDelta ([],[])
    where wAppearance     = express (appearance x)    (appearance y)
          wBrain          = express (brain x)         (brain y)
          wDevotion       = express (devotion x)      (devotion y)
          wAgeOfMaturity  = express (ageOfMaturity x)
                                                      (ageOfMaturity y)
          wPassionDelta   = express (passionDelta x)  (passionDelta y)

instance Agent (Wain ct pt p a m) where
  agentId = name
  isAlive w = energy w > 0

instance (Genetic ct, Diploid ct, Genetic pt, Diploid pt,
          Diploid p, Genetic p, Diploid a, Genetic a, Ord a,
          Diploid m, Genetic m, Muser m)
  => RS.Reproductive (Wain ct pt p a m) where
  type Strand (Wain ct pt p a m) = Sequence
  genome = genome
  produceGamete a =
    repeatWithProbability 0.1 randomCrossover (genome a) >>=
    withProbability 0.01 randomCutAndSplice >>=
    withProbability 0.001 mutatePairedLists >>=
    randomOneOfPair
  build n = runDiploidReader (buildWainFromGenome False n)

--
--
-- Queries (functions that do not modify wains)
--
--

-- | Returns the total energy of all children in the litter.
childEnergy :: Wain ct pt p a m -> Double
childEnergy = sum . map (UI.wide . energy) . litter

-- | Returns @True@ if a wain is currently raising children; returns
--   @False@ otherwise.
hasLitter :: Wain ct pt p a m -> Bool
hasLitter = not . null . litter

-- | Returns the number of children that a wain is currently raising.
litterSize :: Wain ct pt p a m -> Int
litterSize = length . litter

-- | Returns @True@ if the wain is mature; returns @False@ otherwise.
mature :: Wain ct pt p a m -> Bool
mature a = age a >= ageOfMaturity a

-- | Returns the wain's current condition. This is useful for making
--   decisions.
condition :: Wain ct pt p a m -> B.Condition
condition w = [ energy w, 1 - passion w, if l > 0 then 1 else 0 ]
  where l = length . litter $ w

-- | Returns the wain's current happiness level.
--   This is a number between 0 and 1, inclusive.
happiness :: Wain ct pt p a m -> UI.Double
happiness w = B.happiness (brain w) (condition w)

-- | Returns the measure of how novel each input pattern was to the
--   wain.
novelties :: B.DecisionReport p a -> [UI.Double]
novelties = map GSOM.cNovelty . Cl.cDetails . B.bdrClassifierReport

-- | Returns the measure of how novel each input pattern was to the
--   wain, adjusted for the age of the wain.
adjNovelties :: B.DecisionReport p a -> [Int]
adjNovelties = map GSOM.cAdjNovelty . Cl.cDetails . B.bdrClassifierReport

--
--
-- Functions that do modify wains.
--
--

--
-- Energy modification
--

adjustEnergy
  :: Double -> String -> Wain ct pt p a m -> (Wain ct pt p a m, [String])
adjustEnergy delta source w = ( w3, msgs2 ++ msgs3)
  where w1 = recordEvent (AdjustEnergy delta source) w
        (w2, msgs2, delta2) = apportion delta source w1
        (w3, msgs3) = adjustSelfEnergy delta2 source w2

apportion
  :: Double -> String -> Wain ct pt p a m -> (Wain ct pt p a m, [String], Double)
apportion delta source w
  = if null (litter w)
      then (w, [], delta)
      else (w', msgs, carerShare)
  where (w', msgs) = adjustLitterEnergy litterShare source w
        litterShare = delta * UI.wide (devotion w)
        carerShare = delta - litterShare

adjustLitterEnergy
  :: Double -> String -> Wain ct pt p a m -> (Wain ct pt p a m, [String])
adjustLitterEnergy delta source w
  = (recordEvent (AdjustLitterEnergy delta source) w', [msg])
  where w' = w { litter=litter' }
        litter' = map (fst . adjustEnergy childShare source') $ litter w
        source' = source ++ " (from carer)"
        childShare = delta / (fromIntegral . length $ litter w)
        msg = "Shared " ++ pretty childShare ++ " from " ++ source ++ " with offspring"

adjustSelfEnergy
  :: Double -> String -> Wain ct pt p a m -> (Wain ct pt p a m, [String])
adjustSelfEnergy delta source w
  = (recordEvent (AdjustSelfEnergy delta source) w', msgs)
  where w' = w { energy=e' }
        e = energy w
        (e', used, leftover) = calculateEnergyChange delta e
        msgs = [ "Adjusting energy because " ++ source,
                 pretty e ++ " " ++ pretty delta
                   ++ " -> " ++ pretty e'
                   ++ " (used=" ++ pretty used
                   ++ ", leftover=" ++ pretty leftover ++ ")" ]

calculateEnergyChange :: Double -> UI.Double -> (UI.Double, Double, Double)
calculateEnergyChange delta e = (e', used, leftover)
  where e' = UI.crop $ UI.wide e + delta
        used = UI.wide e' - UI.wide e
        leftover = delta - used

--
-- Passion modification
--

-- | Adjusts the wain's passion by the genetically-determined amount.
--   NOTE: The passion is capped to the range [0,1]. The litter is not
--   affected.
autoAdjustPassion :: Wain ct pt p a m -> (Wain ct pt p a m, [String])
autoAdjustPassion w
  = (recordEvent AutoAdjustPassion w', ["Automatically adjusted passion"])
  where w' = w { passion=p }
        p = UI.narrow . enforceRange unitInterval $
              UI.wide (passion w) + UI.wide (passionDelta w)

-- | Internal method. D; do not export.
-- | Resets the wain's passion to zero.
--   This would normally be called immediately after mating.
coolPassion :: Wain ct pt p a m -> (Wain ct pt p a m, [String])
coolPassion w = (recordEvent CoolPassion w', ["Cooled passion"])
  where w' = w { passion=0 }

--
-- Age modification
--

-- | Increments the age of the wain, and its litter (if any).
incAge :: Wain ct pt p a m -> (Wain ct pt p a m, [String])
incAge w = (recordEvent IncAge w', ["Age is now " ++ show (age w')])
  where w' = w { age=age w + 1, litter=litter' }
        litter' = map (fst . incAge) $ litter w
        -- chldren don't have children, so this will terminate

--
-- Litter modification
--

-- | Attempts to mate two wains.
--   If either of the wains already has a litter, mating will not occur.
--   If mating does occur, the passion level of both wains will be
--   reset to zero.
--   Returns the (possibly modified) wains, descriptions of any errors
--   that occurred when attempting to produce a child from the genome,
--   and the energy contribution from each parent.
mate
  :: (Diploid ct, Genetic ct, SOM.Adjuster ct, SOM.PatternType ct ~ p,
     SOM.MetricType ct ~ UI.Double, SOM.TimeType ct ~ Word32,
     Diploid pt, Genetic pt, SOM.Adjuster pt, SOM.PatternType pt ~ R.Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32,
     Diploid p, Genetic p, Pretty p,
     Diploid a, Genetic a, Pretty a, Eq a, Ord a,
     Diploid m, Genetic m, Muser m, Pretty m, Action m ~ a, RandomGen r)
  => Wain ct pt p a m -> Wain ct pt p a m -> String
    -> Rand r ([Wain ct pt p a m], [String])
mate a b babyName
  | hasLitter a
      = return ([a', b'], [name a ++ " already has a litter"])
  | hasLitter b
      = return ([a', b'], [name b ++ " already has a litter"])
  | otherwise = mate' a' b' babyName
  where a' = recordEvent AskToMate a
        b' = recordEvent AgreeToMate b

-- | Internal method
mate'
  :: (Diploid ct, Genetic ct, SOM.Adjuster ct, SOM.PatternType ct ~ p,
     SOM.MetricType ct ~ UI.Double, SOM.TimeType ct ~ Word32,
     Diploid pt, Genetic pt, SOM.Adjuster pt, SOM.PatternType pt ~ R.Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32,
     Diploid p, Genetic p, Pretty p,
     Diploid a, Genetic a, Pretty a, Eq a, Ord a,
     Diploid m, Genetic m, Muser m, Pretty m, Action m ~ a, RandomGen r)
  => Wain ct pt p a m -> Wain ct pt p a m -> String
    -> Rand r ([Wain ct pt p a m], [String])
mate' a b babyName = do
  let a2 = fst $ coolPassion a
  let b2 = fst $ coolPassion b
  result <- RS.makeOffspring a b babyName
  case result of
    Right baby -> do
      let (a3, b3, baby3) = donateParentsEnergy a2 b2 baby
      let (a4, msgs4) = addChild baby3 a3
      return ([a4, b3], msgs4)
    Left msgs -> return ([a2, b2], msgs)

addChild
  :: Wain ct pt p a m -> Wain ct pt p a m -> (Wain ct pt p a m, [[Char]])
addChild c w = (recordEvent (AddChild c) w', [ "Bore child " ++ name c ])
  where w' = w { litter=c:litter w }

-- | Internal method
donateParentsEnergy
  :: (SOM.Adjuster ct, SOM.PatternType ct ~ p,
     SOM.MetricType ct ~ UI.Double, SOM.TimeType ct ~ Word32,
     SOM.Adjuster pt, SOM.PatternType pt ~ R.Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32,
     Pretty p, Pretty a, Eq a, Ord a, Muser m, Pretty m, Action m ~ a)
  => Wain ct pt p a m -> Wain ct pt p a m -> Wain ct pt p a m
     -> (Wain ct pt p a m, Wain ct pt p a m, Wain ct pt p a m)
donateParentsEnergy a b c = (a', b', c')
  where aContribution = - UI.wide (devotion a * energy a)
        bContribution = - UI.wide (devotion b * energy b)
        a' = fst $ adjustSelfEnergy aContribution "donation to child" a
        b' = fst $ adjustSelfEnergy bContribution "donation to child" b
        cContribution = -(aContribution + bContribution)
        c' = fst $ adjustSelfEnergy cContribution "donation from parents" c

-- | Removes any mature children from the wain's litter.
--   Returns a list containing the (possibly modified) wain, together
--   with any children that have just been weaned.
weanMatureChildren :: Wain ct pt p a m -> ([Wain ct pt p a m], [String])
weanMatureChildren w =
  if null (litter w)
    then ([w], [])
    else (w':weanlings, msgs)
  where weanlings = map (fst . markMature) . filter mature $ litter w
        (w', msgs) = foldl' (removeChildFold "it has matured") (w, []) weanlings

-- | Internal method
removeChildFold
  :: String -> (Wain ct pt p a m, [String]) -> Wain ct pt p a m
  -> (Wain ct pt p a m, [String])
removeChildFold cause (w, msgs) child = (w', msgs ++ msgs')
  where (w', msgs') = removeChild child cause w

-- | Internal method
removeChild
  :: Wain ct pt p a m -> String -> Wain ct pt p a m -> (Wain ct pt p a m, [String])
removeChild child cause w
  = (recordEvent (RemoveChild child cause) w', [msg])
  where w' = w { litter=filter wrongName (litter w) }
        wrongName c = name c /= name child
        msg = "Separated from child " ++ name child ++ " because " ++ cause

-- | Internal method
markMature :: Wain ct pt p a m -> (Wain ct pt p a m, [String])
markMature w = (recordEvent Mature w,
                ["Mature at age " ++ show (age w)])

-- | Removes any dead children from the wain's litter.
pruneDeadChildren :: Wain ct pt p a m -> ([Wain ct pt p a m], [String])
pruneDeadChildren w =
  if null (litter w)
    then ([w], [])
    else (w':deadChildren, msgs)
  where deadChildren = map (fst . recordDeath "zero energy") . filter (not . isAlive) $ litter w
        (w', msgs) = foldl' (removeChildFold "it died") (w, []) deadChildren

-- | Internal method
recordDeath :: String -> Wain ct pt p a m -> (Wain ct pt p a m, [String])
recordDeath cause w = (recordEvent (Death cause) w',
                        ["Died at age " ++ show (age w'),
                         "Cause of death: " ++ cause])
  where w' = w { litter=litter' }
        litter' = map (fst . recordDeath "death of carer") $ litter w

--
-- Brain modification
--

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
  :: ( SOM.Adjuster ct, SOM.PatternType ct ~ p, SOM.MetricType ct ~ UI.Double,
      SOM.TimeType ct ~ Word32, SOM.PatternType pt ~ R.Response a,
      SOM.Adjuster pt, SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32,
      Pretty p, Ord a, Pretty a, Muser m, Pretty m, a ~ Action m)
  => [p] -> Wain ct pt p a m -> (Wain ct pt p a m, a, [String])
chooseAction ps w = (recordEvent (ChooseAction ps) w', a, msgs)
  where w' = w { brain=b', litter=litter' }
        (dReport, b') = B.chooseAction (brain w) ps (condition w)
        msgs = B.prettyDecisionReport dReport
        r = B.bdrRecommendedResponse dReport
        a = R.action r
        litter' = map (first . chooseAction ps) (litter w)
        first (x, _, _) = x
          -- actions chosen by children are only used for reflection

-- | Causes a wain to considers whether it is happier or not as a
--   result of the last action it took, and modifies its decision models
--   accordingly. The wain's litter, if any, will not have access to
--   the parent's internal condition, and their own condition will not
--   change, so they do not have any way to assess whether the outcome
--   of the action was good. Instead they will simply assume that
--   the action was perfect (increased happiness by 1).
--   TODO: Do something more realistic.
reflect
  :: (SOM.Adjuster pt, SOM.PatternType pt ~ R.Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32,
     Eq a, Pretty a)
  => Wain ct pt p a m -> (Wain ct pt p a m, [String])
reflect w = (recordEvent Reflect w', msgs)
  where w' = w { brain=b', litter=litter' }
        (rReport, b') = B.reflect (brain w) (condition w)
        litter' = map (fst . reflect) $ litter w
        msgs = maybe ["No action to reflect on"] B.prettyReflectionReport rReport

-- | Teaches the wain a desirable action to take in response to a
--   stimulus.
--   This can be used to help children learn by observing their parents.
--   It can also be used to allow wains to learn from others.
--   Returns a detailed report of the imprint process
--   and the updated wain.
imprintResponse
  :: (SOM.Adjuster ct, SOM.PatternType ct ~ p,
     SOM.MetricType ct ~ UI.Double, SOM.TimeType ct ~ Word32,
     SOM.Adjuster pt, SOM.PatternType pt ~ R.Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32,
     Eq a, Pretty a)
  => [p] -> a -> Wain ct pt p a m -> (Wain ct pt p a m, [String])
imprintResponse ps a w
  = (recordEvent (ImprintResponse ps a) w', msgs)
  where w' = w { brain=b'' }
        (iReport, b'') = B.imprintResponse b' ls a
        (cReport, b') = B.classifyInputs (brain w) ps
        ls = map GSOM.cBmu $ Cl.cDetails cReport
        msgs = P.prettyLearningReport iReport

--
-- Miscelaneous utilities
--

-- TODO: Do I still need all these?

data ReflectionReport p a
  = ReflectionReport
      {
        rReflectionReport :: B.ReflectionReport a,
        rImprintReports   :: [P.LearningReport a]
      } deriving (Generic, Show, NFData)

happinessError :: ReflectionReport p a -> Double
happinessError = B.brrErr . rReflectionReport


-- | Describes the classifier models.
prettyClassifierModels :: Pretty p => Wain ct pt p a m -> [String]
prettyClassifierModels w
  = prettyClassifierModels' w
      ++ concatMap (indent . prettyClassifierModels') (litter w)

prettyClassifierModels' :: Pretty p => Wain ct pt p a m -> [String]
prettyClassifierModels' w
  = (agentId w ++ "'s classifier models") : map f ms
  where ms = M.toList . SOM.modelMap . B.classifier . brain $ w
        f (l, m) = show l ++ " " ++ pretty m

-- | Describes the predictor models.
prettyPredictorModels :: Pretty a => Wain ct pt p a m -> [String]
prettyPredictorModels w
  = prettyPredictorModels' w
      ++ concatMap (indent . prettyPredictorModels') (litter w)

prettyPredictorModels' :: Pretty a => Wain ct pt p a m -> [String]
prettyPredictorModels' w
  = (agentId w ++ "'s predictor models") : map f ms
  where ms = M.toList . SOM.modelMap . B.predictor . brain $ w
        f (l, m) = show l ++ ": " ++ pretty m

-- | Generates a report that shows relationships between the classifier
--   and predictor models.
--   Useful when the classifier models and the predictor models are
--   both brief.
prettyBrainSummary :: (Pretty p, Pretty a) => Wain ct pt p a m -> [String]
prettyBrainSummary w
  = (agentId w ++ "'s brain summary") : map f rs
  where ps = M.toList . SOM.modelMap . B.classifier . brain $ w
        rs = sortOn (R.labels . snd) . M.toList
               . SOM.modelMap . B.predictor . brain $ w
        f (l, r) = show l ++ ": "
                     ++ intercalate "," (map g (R.labels r)) ++ " "
                     ++ pretty r
        g l' = show l' ++ " " ++ pretty (lookup l' ps)

indent :: [String] -> [String]
indent = map ("...." ++ )

instance (SOM.Adjuster ct, Report ct, SOM.Adjuster pt, Report pt,
           Pretty p, Eq a, Pretty a, Pretty m)
  => Report (Wain ct pt p a m) where
  report w = report' w
             ++ map ("child " ++) childReport
    where childReport = concatMap report' (litter w)

report'
  :: (SOM.Adjuster ct, Report ct, SOM.Adjuster pt, Report pt,
           Pretty p, Eq a, Pretty a, Pretty m)
  => Wain ct pt p a m -> [String]
report' w = map (\s -> n ++ ' ':s) xs
  where n = name w
        xs = [ "appearance: " ++ pretty (appearance w),
               "devotion: " ++ printf "%5.3f" (UI.wide $ devotion w),
               "ageOfMaturity: " ++ printf "%d" (ageOfMaturity w),
               "passionDelta: " ++ pretty (passionDelta w),
               "energy: " ++ printf "%5.3f" (UI.wide $ energy w),
               "passion: " ++ printf "%5.3f" (UI.wide $ passion w),
               "age: " ++ pretty (age w),
               "children: " ++ childrensNames ]
             ++ report (brain w)
        childrensNames = intercalate ", " $ map name (litter w)

