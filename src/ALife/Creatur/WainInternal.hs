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
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module ALife.Creatur.WainInternal where

import           ALife.Creatur                              (Agent, agentId,
                                                             isAlive)
import           ALife.Creatur.Database                     (Record,
                                                             SizedRecord, key)
import qualified ALife.Creatur.Database                     (size)
import qualified ALife.Creatur.Gene.Numeric.UnitInterval    as UI
import           ALife.Creatur.Gene.Numeric.Util            (enforceRange,
                                                             unitInterval)
import           ALife.Creatur.Genetics.BRGCWord8           (DiploidReader,
                                                             Genetic, Sequence,
                                                             consumed2, copy,
                                                             copy2, get,
                                                             getAndExpress, put,
                                                             runDiploidReader,
                                                             write)
import           ALife.Creatur.Genetics.Diploid             (Diploid, express)
import           ALife.Creatur.Genetics.Recombination       (mutatePairedLists,
                                                             randomCrossover,
                                                             randomCutAndSplice,
                                                             randomOneOfPair,
                                                             repeatWithProbability,
                                                             withProbability)
import qualified ALife.Creatur.Genetics.Reproduction.Sexual as RS
import qualified ALife.Creatur.Wain.Brain                   as B
import qualified ALife.Creatur.Wain.Classifier              as Cl
import qualified ALife.Creatur.Wain.GeneticSOM              as GSOM
import           ALife.Creatur.Wain.Muser                   (Action, Muser)
import qualified ALife.Creatur.Wain.Predictor               as P
import           ALife.Creatur.Wain.Pretty                  (Pretty, pretty)
import           ALife.Creatur.Wain.Report                  (Report, report)
import qualified ALife.Creatur.Wain.Response                as R
import           ALife.Creatur.Wain.Statistics              (Statistical, dStat,
                                                             iStat, stats)
import           Control.DeepSeq                            (NFData)
import           Control.Monad.Random                       (Rand, RandomGen)
import qualified Data.Datamining.Clustering.SGM4Internal    as SOM
import           Data.List                                  (intercalate,
                                                             partition, sortOn)
import qualified Data.Map.Strict                            as M
import           Data.Serialize                             (Serialize)
import           Data.Version                               (showVersion)
import           Data.Word                                  (Word16, Word32,
                                                             Word8)
import           GHC.Generics                               (Generic)
import           Paths_creatur_wains                        (version)
import           Text.Printf                                (printf)

-- | Returns the current version number of this library.
packageVersion :: String
packageVersion = "creatur-wains-" ++ showVersion version

-- Anything that modifies a wain is an event.
data Event p = IncAge
             -- | Adjusts the energy of a wain.
             --   NOTE: A wain's energy is capped to the range [0,1],
             --   so the actual change in energy may be less than
             --   the specified amount.
             | EnergyAdjustment Double String
             | ChooseAction [p]
             deriving (Eq, Show, Read, Generic, NFData, Serialize)

-- | A data mining agent that can learn, reproduce, and evolve.
data Wain ct pt p a m = Wain
  {
    -- | Each wain should have a unique name.
    name                   :: String,
    -- | A wain's appearance is a pattern by which other wains can
    --   recognise it as a fellow wain.
    appearance             :: p,
    -- | The wain's brain, which recognises patterns and makes
    --   decisions.
    brain                  :: B.Brain ct pt p a m,
    -- | The amount of energy the wain will give to offspring at birth.
    --   This is a number between 0 and 1, inclusive.
    devotion               :: UI.UIDouble,
    -- | The age at which this wain will/has left its parent.
    ageOfMaturity          :: Word16,
    -- | The amount that a wain's passion increases at each CPU turn.
    --   this influences the frequency of mating.
    passionDelta           :: UI.UIDouble,
    -- | The wain's current energy level.
    --   This is a number between 0 and 1, inclusive.
    energy                 :: UI.UIDouble,
    -- | The wain's current passion level
    --   This is a number between 0 and 1, inclusive.
    passion                :: UI.UIDouble,
    -- | The wain's current age.
    age                    :: Word16,
    -- | The children this wain is currently rearing.
    litter                 :: [Wain ct pt p a m],
    -- | The number of children this wain has borne.
    childrenBorneLifetime  :: Word16,
    -- | The number of children this wain has reared to maturity.
    childrenWeanedLifetime :: Word16,
    -- | The wain's genes.
    genome                 :: ([Word8],[Word8]),
    -- | The events that occurred during the wain's life.
    biography              :: [Event p]
  } deriving (Eq, Generic, NFData)

deriving instance (Show ct, Show pt, Show p, Show a, Show m)
  => Show (Wain ct pt p a m)

-- | Internal method
buildWain
  :: String -> p -> B.Brain ct pt p a m -> UI.UIDouble -> Word16
  -> UI.UIDouble -> (Sequence, Sequence) -> Wain ct pt p a m
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
        childrenBorneLifetime = 0,
        childrenWeanedLifetime = 0,
        genome = g,
        biography  = []
      }

-- | Constructs a wain with the specified parameters, with the
--   corresponding (generated) genome. This would normally be used only
--   for generating the initial population.
buildWainAndGenerateGenome
  :: (Genetic ct, Genetic pt, Genetic p, Genetic a, Genetic m, Muser m)
  => String -> p -> B.Brain ct pt p a m -> UI.UIDouble -> Word16
  -> UI.UIDouble -> Wain ct pt p a m
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
          iStat "children borne (lifetime)" (childrenBorneLifetime w),
          iStat "children reared (lifetime)" (childrenWeanedLifetime w),
          dStat "adult energy" (UI.wide e),
          dStat "child energy" (UI.wide ec),
          iStat "genome length" ( (length . fst . genome $ w)
                                  + (length . snd . genome $ w) )]
    where e = energy w
          ec = sum . map (energy) $ litter w

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


recordEvent :: Event p -> Wain ct pt p a m -> Wain ct pt p a m
recordEvent e w = w { biography=e:biography w }

runEvent :: Event p -> Wain ct pt p a m -> Wain ct pt p a m
runEvent e =  justRunEvent e . recordEvent e

justRunEvent :: Event p -> Wain ct pt p a m -> Wain ct pt p a m
justRunEvent _ _ = undefined


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
happiness :: Wain ct pt p a m -> UI.UIDouble
happiness w = B.happiness (brain w) (condition w)

--numModels . B.classifier . brain $ w

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
novelties :: DecisionReport p a -> [UI.UIDouble]
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
  :: ( SOM.Adjuster ct, SOM.PatternType ct ~ p, SOM.MetricType ct ~ UI.UIDouble,
      SOM.TimeType ct ~ Word32, SOM.PatternType pt ~ R.Response a,
      SOM.Adjuster pt, SOM.MetricType pt ~ UI.UIDouble, SOM.TimeType pt ~ Word32,
      Ord a, Muser m, Pretty m, a ~ Action m)
  => [p] -> Wain ct pt p a m -> (DecisionReport p a, R.Response a, Wain ct pt p a m)
chooseAction ps w = (dReport', r, w' { litter=litter' })
  where (dReport, b')
          = B.chooseAction (brain w) ps (condition w)
        r = B.bdrRecommendedResponse dReport
        w' = recordEvent (ChooseAction ps) $ w { brain=b' }
        ls = R.labels r
        lps = zip ls ps
        iResults = map (imprintStimulus lps) (litter w')
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

adjustEnergy
  :: Double -> Wain ct pt p a m -> (Wain ct pt p a m, Double)
adjustEnergy delta w = (wAfter, delta')
  where eBefore = energy w
        eAfter = UI.crop $ UI.wide (energy w) + delta
        wAfter = w { energy=eAfter }
        delta' = UI.wide eAfter - UI.wide eBefore

-- | Adjusts the wain's passion by the genetically-determined amount.
--   NOTE: The passion is capped to the range [0,1]. The litter is not
--   affected.
autoAdjustPassion :: Wain ct pt p a m -> Wain ct pt p a m
autoAdjustPassion w = w { passion=p }
  where p = UI.narrow . enforceRange unitInterval $
              UI.wide (passion w) + UI.wide (passionDelta w)

-- | Resets the wain's passion to zero.
--   This would normally be called immediately after mating.
coolPassion :: Wain ct pt p a m -> Wain ct pt p a m
coolPassion w = w { passion=0 }

-- | Increments the age of the wain, and its litter (if any).
incAge :: Wain ct pt p a m -> Wain ct pt p a m
incAge = incAge1 . incLitterAge

-- | Internal method
incLitterAge :: Wain ct pt p a m -> Wain ct pt p a m
incLitterAge w = w { litter=litter' }
  where litter' = map incAge1 $ litter w

-- | Internal method
incAge1 :: Wain ct pt p a m -> Wain ct pt p a m
incAge1 w = w { age=age w + 1 }

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
  :: (SOM.Adjuster pt, SOM.PatternType pt ~ R.Response a,
     SOM.MetricType pt ~ UI.UIDouble, SOM.TimeType pt ~ Word32, Eq a)
  => R.Response a -> Wain ct pt p a m -> Wain ct pt p a m
  -> (ReflectionReport p a, Wain ct pt p a m)
reflect r wBefore wAfter =
  (rReport', wReflected { litter=litter' })
  where (rReport, wReflected) = reflect1 r wBefore wAfter
        a = R.action r
        ls = R.labels r
        iResults = map (imprintResponse ls a) (litter wAfter)
        litter' = map snd iResults
        rReport' = ReflectionReport
                   {
                     rReflectionReport = rReport,
                     rImprintReports = map fst iResults
                   }

-- | Internal method
reflect1
  :: (SOM.Adjuster pt, SOM.PatternType pt ~ R.Response a,
     SOM.MetricType pt ~ UI.UIDouble, SOM.TimeType pt ~ Word32, Eq a)
  => R.Response a -> Wain ct pt p a m -> Wain ct pt p a m
  -> (B.ReflectionReport a, Wain ct pt p a m)
reflect1 r wBefore wAfter = (rReport, wAfter { brain=b' })
  where (rReport, b') = B.reflect (brain wAfter) r (condition wBefore)
                        (condition wAfter)

type StimulusImprintReport p = [GSOM.ImprintDetail p]

-- | Teaches the wain a set of patterns, and a label for each of them.
imprintStimulus
  :: (SOM.Adjuster ct, SOM.PatternType ct ~ p,
     SOM.MetricType ct ~ UI.UIDouble, SOM.TimeType ct ~ Word32)
  => [(GSOM.Label, p)] -> Wain ct pt p a m
  -> (StimulusImprintReport p, Wain ct pt p a m)
imprintStimulus lps w = (iReport, w { brain=b' })
  where (iReport, b') = B.imprintStimulus (brain w) lps

type ResponseImprintReport a = P.LearningReport a

-- | Teaches the wain a desirable action to take in response to a
--   stimulus.
--   This can be used to help children learn by observing their parents.
--   It can also be used to allow wains to learn from others.
--   Returns a detailed report of the imprint process
--   and the updated wain.
imprintResponse
  :: (SOM.Adjuster pt, SOM.PatternType pt ~ R.Response a,
     SOM.MetricType pt ~ UI.UIDouble, SOM.TimeType pt ~ Word32, Eq a)
  => [GSOM.Label] -> a -> Wain ct pt p a m -> (P.LearningReport a, Wain ct pt p a m )
imprintResponse ls a w = (iReport, w { brain=b' })
  where (iReport, b') = B.imprintResponse (brain w) ls a

-- | Attempts to mate two wains.
--   If either of the wains already has a litter, mating will not occur.
--   If mating does occur, the passion level of both wains will be
--   reset to zero.
--   Returns the (possibly modified) wains, descriptions of any errors
--   that occurred when attempting to produce a child from the genome,
--   and the energy contribution from each parent.
mate
  :: (Diploid ct, Genetic ct, Diploid pt, Genetic pt,
     Genetic p, Diploid p, Diploid a, Genetic a, Ord a,
     Diploid m, Genetic m, Muser m, RandomGen r)
  => Wain ct pt p a m -> Wain ct pt p a m -> String
  -> Rand r ([Wain ct pt p a m], [String], Double, Double)
mate a b babyName
  | hasLitter a
      = return ([a, b], [name a ++ " already has a litter"], 0, 0)
  | hasLitter b
      = return ([a, b], [name b ++ " already has a litter"], 0, 0)
  | otherwise = mate' a b babyName

-- | Internal method
mate'
  :: (Diploid ct, Genetic ct, Diploid pt, Genetic pt,
     Genetic p, Diploid p, Diploid a, Genetic a, Ord a,
     Diploid m, Genetic m, Muser m, RandomGen r)
  => Wain ct pt p a m -> Wain ct pt p a m -> String
   -> Rand r ([Wain ct pt p a m], [String], Double, Double)
mate' a b babyName = do
  let a2 = coolPassion a
  let b2 = coolPassion b
  result <- RS.makeOffspring a b babyName
  case result of
    Right baby -> do
      let (a3, b3, baby3, aContribution, bContribution)
             = donateParentEnergy a2 b2 baby
      let a4 = a3 { litter=[baby3],
                    childrenBorneLifetime=childrenBorneLifetime a3 + 1 }
      return ([a4, b3], [], aContribution, bContribution)
    Left msgs -> return ([a2, b2], msgs, 0, 0)

-- | Internal method
donateParentEnergy
  :: Wain ct pt p a m -> Wain ct pt p a m -> Wain ct pt p a m
     -> (Wain ct pt p a m, Wain ct pt p a m, Wain ct pt p a m,
          Double, Double)
donateParentEnergy a b c = (a', b', c', aContribution', bContribution')
  where aContribution = - UI.wide (devotion a * energy a)
        bContribution = - UI.wide (devotion b * energy b)
        (a', aContribution') = adjustEnergy aContribution a
        (b', bContribution') = adjustEnergy bContribution b
        cContribution = -(aContribution' + bContribution')
        (c', _) = adjustEnergy cContribution c

-- | Removes any mature children from the wain's litter.
--   Returns a list containing the (possibly modified) wain, together
--   with any children that have just been weaned.
weanMatureChildren :: Wain ct pt p a m -> [Wain ct pt p a m]
weanMatureChildren a =
  if null (litter a)
    then [a]
    else a':weanlings
  where (weanlings, babes) = partition mature (litter a)
        newWeanlings = fromIntegral $ length weanlings
        a' = a { litter=babes,
                 childrenWeanedLifetime=childrenWeanedLifetime a + newWeanlings }

-- | Removes any dead children from the wain's litter.
pruneDeadChildren :: Wain ct pt p a m -> [Wain ct pt p a m]
pruneDeadChildren a =
  if null (litter a)
    then [a]
    else a':deadChildren
  where (aliveChildren, deadChildren) = partition isAlive (litter a)
        a' = a { litter=aliveChildren }

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

prettyClassificationReport
  :: Pretty p
  => Wain ct pt p a m -> DecisionReport p a -> [String]
prettyClassificationReport w r
  = (agentId w ++ " classifies the input(s)")
    : Cl.prettyClassifierReport (wdrClassifierReport r)
    ++ concatMap f wrs
  where wrs = zip (litter w) (wdrImprintReports r)
        f (child, iReport) = indent $
                               prettyStimulusImprintReport child iReport

prettyScenarioReport
  :: Wain ct pt p a m -> DecisionReport p a -> [String]
prettyScenarioReport w r
  = (agentId w ++ " develops hypotheses about the current scenario")
    : B.prettyScenarioReport (wdrScenarioReport r)

prettyPredictionReport
  :: Pretty a
  => Wain ct pt p a m -> DecisionReport p a -> [String]
prettyPredictionReport w r
  = (agentId w ++ " considers possible responses")
    : map pretty (wdrPredictorReport r)

prettyActionReport
  :: Pretty a
  => Wain ct pt p a m -> DecisionReport p a -> [String]
prettyActionReport w r
  = (agentId w ++ " predicts outcomes of possible responses")
    : B.prettyActionReport (wdrActionReport r)

prettyReflectionReport
  :: Pretty a
  => Wain ct pt p a m -> ReflectionReport p a -> [String]
prettyReflectionReport w r
  = (agentId w ++ " reflects on the outcome")
    : B.prettyReflectionReport (rReflectionReport r)
    ++ concatMap f wrs
  where wrs = zip (litter w) (rImprintReports r)
        f (child, iReport) = indent $
                               prettyResponseImprintReport child iReport

prettyStimulusImprintReport
  :: Pretty p
  => Wain ct pt p a m -> StimulusImprintReport p -> [String]
prettyStimulusImprintReport w r
  = (agentId w ++ " imprints a stimulus")
    : concatMap GSOM.prettyImprintDetail r

prettyResponseImprintReport
  :: Pretty a => Wain ct pt p a m -> ResponseImprintReport a -> [String]
prettyResponseImprintReport w r
  = (agentId w ++ " imprints a response")
    : P.prettyLearningReport r

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
               "total # children borne: "
                 ++ pretty (childrenBorneLifetime w),
               "total # children weaned: "
                 ++ pretty (childrenWeanedLifetime w),
               "children: " ++ childrensNames ]
             ++ report (brain w)
        childrensNames = intercalate ", " $ map name (litter w)

