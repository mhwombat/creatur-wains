-----------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.BrainInternal
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private Brain internals.
-- Most developers should use Brain instead.
-- This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ALife.Creatur.Wain.BrainInternal where

import qualified ALife.Creatur.Gene.Numeric.PlusMinusOne as PM1
import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import           ALife.Creatur.Gene.Numeric.Weights      (Weights, numWeights,
                                                          weightAt, weightedSum)
import           ALife.Creatur.Genetics.BRGCWord8        (Genetic, get, put)
import           ALife.Creatur.Genetics.Diploid          (Diploid)
import qualified ALife.Creatur.Wain.Classifier           as Cl
import qualified ALife.Creatur.Wain.GeneticSOM           as GSOM
import qualified ALife.Creatur.Wain.Muser                as M
import qualified ALife.Creatur.Wain.Predictor            as P
import           ALife.Creatur.Wain.Pretty               (Pretty, pretty)
import           ALife.Creatur.Wain.Probability          (hypothesise,
                                                          prettyProbability)
import           ALife.Creatur.Wain.Report               (Report, report)
import           ALife.Creatur.Wain.Response             (Response (..))
import           ALife.Creatur.Wain.Statistics           (Statistical, dStat,
                                                          iStat, prefix, stats)
import           Control.DeepSeq                         (NFData)
import qualified Data.Datamining.Clustering.SGM4         as SOM
import           Data.Function                           (on)
import           Data.List                               (foldl', groupBy,
                                                          sortBy)
import qualified Data.Map.Strict                         as M
import           Data.Ord                                (comparing)
import           Data.Serialize                          (Serialize)
import           Data.Word                               (Word32, Word8)
import           GHC.Generics                            (Generic)
import           Text.Printf                             (printf)

-- | A wain's condition
type Condition = [UI.Double]

-- | A brain which recommends reponses to stimuli, and learns from the
--   outcomes.
data Brain ct pt p a m = Brain
  {
    -- | Component that categorises and identifies patterns
    classifier          :: Cl.Classifier ct p,
    -- | Component that generates response models for consideration
    muser               :: m,
    -- | Component that decides what actions to take
    predictor           :: P.Predictor pt a,
    -- | Weights for evaluating happiness
    happinessWeights    :: Weights,
    -- | Used to break ties when actions seem equally promising
    tiebreaker          :: Word8,
    -- | Controls how willing the wain is to consider alternative
    --   classifications when making decisions.
    --   Must be >= 1.
    strictness          :: Word32,
    -- | When a wain observes a response that it has never seen before,
    --   it will assume the action has the following outcomes.
    --   Normally these values should all be positive.
    imprintOutcomes     :: [PM1.Double],
    -- | When a wain observes a response that it already knows, it will
    --   reinforce it by re-learning the model augmented with these
    --   delta outcomes.
    --   Normally these values should all be positive.
    reinforcementDeltas :: [PM1.Double],
    -- | Number of times each action has been used
    actionCounts        :: M.Map a Int,
    -- | Last choice made
    lastChoice          :: Maybe (Response a, Condition)
  } deriving (Generic, Eq, NFData)

-- | @'makeBrain' c m p hw t ios rds@ returns a brain with
--   classifier @c@, muser @m@, predictor @p@, happiness weights @hw@,
--   tiebreaker @t@, imprint outcomes @ios@, and reinforcement deltas
--   @rds@. See @Brain@ for an explanation of these parameters.
makeBrain
  :: M.Muser m
     => Cl.Classifier ct p -> m -> P.Predictor pt a -> Weights -> Word8
       -> Word32 -> [PM1.Double] -> [PM1.Double]
       -> Either [String] (Brain ct pt p a m)
makeBrain c m p hw t x ios rds
  | x < 1
      = Left ["strictness < 1"]
  | numWeights hw /= 3
      = Left ["incorrect number of happiness weights"]
  | length (M.defaultOutcomes m) /= 3
      = Left ["incorrect number of default outcomes"]
  | length ios /= 3
      = Left ["incorrect number of imprint outcomes"]
  | length rds /= 3
      = Left ["incorrect number of reinforcement deltas"]
  | otherwise
      = Right $ Brain c m p hw t x ios rds M.empty Nothing

instance (Serialize ct, Serialize pt, Serialize p, Serialize a, Ord a,
          Serialize m)
  => Serialize (Brain ct pt p a m)

instance (Diploid ct, Diploid pt, Diploid p, Diploid a, Ord a, Diploid m)
  => Diploid (Brain ct pt p a m)

instance (Statistical ct, SOM.Adjuster ct,
          Statistical pt, SOM.Adjuster pt, Eq a, Statistical m)
  => Statistical (Brain ct pt p a m) where
  stats b@(Brain c m p hw t s ios rds _ _)
    | length ios < 3 = error "ios not long enough"
    | length rds < 3 = error "rds not long enough"
    | otherwise =
      map (prefix "classifier ") (stats c)
        ++ stats m
        ++ map (prefix "predictor ") (stats p)
        ++ [ iStat "DQ" $ decisionQuality b,
             dStat "energyWeight" . UI.wide $ hw `weightAt` 0,
             dStat "passionWeight" . UI.wide $ hw `weightAt` 1,
             dStat "litterSizeWeight" . UI.wide $ hw `weightAt` 2,
             dStat "energyImprint" . PM1.wide $ head ios,
             dStat "passionImprint" . PM1.wide $ ios !! 1,
             dStat "litterSizeImprint" . PM1.wide $ ios !! 2,
             dStat "energyReinforcement" . PM1.wide $ head rds,
             dStat "passionReinforcement" . PM1.wide $ rds !! 1,
             dStat "litterSizeReinforcement" . PM1.wide $ rds !! 2,
             iStat "tiebreaker" t, iStat "strictness" s ]

instance (Show ct, Show pt, Show p, Show a, Show m)
  => Show (Brain ct pt p a m) where
  show (Brain c m p hw t s ios rds ks lrc) = "Brain (" ++ show c ++ ") ("
    ++ show m ++ ") (" ++ show p ++ ") (" ++ show hw ++ ") "
    ++ show t ++ " " ++ show s ++ " " ++ show ios ++ show rds
    ++ " (" ++ show ks ++ ") " ++ show lrc

instance (Genetic ct, Genetic pt, Genetic p, Genetic a, Genetic m, M.Muser m)
  => Genetic (Brain ct pt p a m) where
    put (Brain c m p hw t s ios rds _ _)
      = put c >> put m >> put p >> put hw >> put t >> put s >> put ios
          >> put rds
    get = do
      c <- get
      m <- get
      p <- get
      hw <- get
      t <- get
      s <- get
      ios <- get
      rds <- get
      -- Use the safe constructor!
      case makeBrain <$> c <*> m <*> p <*> hw <*> t <*> s <*> ios
             <*> rds of
        Left msgs -> return $ Left msgs
        Right b   -> return b

-- | Detailed information about how a decision was made.
data DecisionReport p a =
  DecisionReport
    {
      bdrStimulus            :: [p],
      bdrClassifierReport    :: Cl.ClassifierReport p,
      bdrScenarioReport      :: ScenarioReport,
      bdrPredictorReport     :: PredictorReport a,
      bdrActionReport        :: ActionReport a,
      bdrRecommendedResponse :: Response a
    } deriving (Generic, Show, NFData)

prettyDecisionReport
  :: (Pretty p, Pretty a)
  => DecisionReport p a -> [String]
prettyDecisionReport r
  = "Classifying inputs:"
        : Cl.prettyClassifierReport (bdrClassifierReport r)
  ++ "Developing hypotheses about current scenario:"
        : prettyScenarioReport (bdrScenarioReport r)
  ++ "Imagining responses:" : map pretty (bdrPredictorReport r)
  ++ "Predicting outcomes:" : prettyActionReport (bdrActionReport r)
  ++ ["Recommending response: " ++ pretty (bdrRecommendedResponse r)]

-- | A set of hypotheses about the scenario the wain is
--   facing, paired with the estimated probability that each
--   hypothesis is true.
--   (A hypothesis is a set of classifier labels.)
type ScenarioReport = [([GSOM.Label], UI.Double)]

-- | Generates a human readable summary of a stimulus.
prettyScenarioReport :: ScenarioReport -> [String]
prettyScenarioReport = map f
  where f (ls, p) = "scenario: " ++ pretty ls ++ " prob: " ++ prettyProbability p

-- | Contains each response considered by the brain
--   (with the predicted outcome filled in),
--   the probability associated with the scenario
--   that the response was based on,
--   the adjusted probability based on how well the
--   predictor model matches the proposed response,
--   the label of the predictor model that best matches
--   the response, and the unadjusted outcomes from
--   the model.
type PredictorReport a = [P.PredictionDetail a]

-- | For each action, the expected outcomes and resulting happiness
type ActionReport a = [(a, [PM1.Double], UI.Double)]

-- | Generates a human readable summary of a decision.
prettyActionReport :: Pretty a => ActionReport a -> [String]
prettyActionReport = map f
  where f (a, os, h) = "predicted outcomes of " ++ pretty a ++ " are "
          ++ unwords (map (printf "%.3f" . UI.wide) os)
          ++ " with resulting happiness "
          ++ printf "%.3f" (UI.wide h)

-- | Chooses a response based on the classification of the stimulus
--   (input patterns) and the wain's condition.
--   Returns:
--
--   * a detailed report of the decision-making process
--   * the chosen response
--   * the updated brain
--
--   NOTE: The response chosen might be a response modelled on
--   a different scenario than the one we think we're in.
--   This might happen, for example, if the ideal response to the
--   most likely scenario has a somewhat good outcome, but the same
--   response to a somewhat likely alternative scenario has a really
--   bad outcome. "I think that food is edible, but I'm not going to
--   eat it just in case I've misidentified it and it's poisonous."
chooseAction
  :: (SOM.Adjuster ct, SOM.PatternType ct ~ p,
     SOM.MetricType ct ~ UI.Double, SOM.TimeType ct ~ Word32,
     SOM.Adjuster pt, SOM.PatternType pt ~ Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32,
     Eq a, Ord a, M.Muser m, Pretty m, a ~ M.Action m)
  => Brain ct pt p a m -> [p] -> Condition
  -> (DecisionReport p a, Brain ct pt p a m)
chooseAction b ps c = (dReport, b4 { lastChoice=Just (r, c) })
  where (cReport, b2) = classifyInputs b ps
        -- cReport = The classifier report.
        -- b2  = the updated brain after classification
        cBmus = Cl.bmus cReport
        -- cBmus = the labels of the (possibly new) models that are
        --         closest to each input pattern
        sReport = generateScenarios b2 cReport
        pReport = generateResponses b2 sReport
        aReport = evaluateActions b2 c pReport
        (a, os, _) = chooseAny b . maximaBy (\(_,_,x) -> x) $ aReport
        -- a = the action that we predict will give the best outcome
        -- os = the expected outcomes
        b3 = adjustActionCounts b2 a
        b4 = pruneObsoleteResponses b3
        r = Response cBmus a os
        dReport = DecisionReport
                   {
                     bdrStimulus = ps,
                     bdrClassifierReport = cReport,
                     bdrScenarioReport = sReport,
                     bdrPredictorReport = pReport,
                     bdrActionReport = aReport,
                     bdrRecommendedResponse = r
                   }

-- | Internal method
generateScenarios
  :: Brain ct pt p a m -> Cl.ClassifierReport p -> ScenarioReport
generateScenarios b cReport = sps
  where ldss = Cl.diffs cReport
        -- ldss = the SGM labels paired with the difference between the
        --       inputs and the corresponding model
        sps = errorIfNull "sps" $ hypothesise (strictness b) ldss
        --   sps = set of hypotheses about the scenario the wain is
        --   facing, paired with the estimated probability that each
        --   hypothesis is true. (A hypothesis is a set of labels.)
--         sps' = filter (P.hasScenario (predictor b) . fst) sps

-- | Internal method
generateResponses
  :: (SOM.Adjuster pt, SOM.PatternType pt ~ Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32,
     M.Muser m, Eq a, a ~ M.Action m)
  => Brain ct pt p a m -> ScenarioReport -> PredictorReport a
generateResponses b sReport = errorIfNull "pReport" $ predictAll b rps
  where as = P.actions $ predictor b
        -- as = list of actions to evaluate
        rps = errorIfNull "rps" $ M.generateResponses (muser b) as sReport
        -- rps = list of responses to consider, paired with the
        --       probability that the response is based on the correct
        --       scenario.


-- | Internal method
evaluateActions
  :: Eq a
  => Brain ct pt p a m -> Condition -> PredictorReport a -> ActionReport a
evaluateActions b c pReport
  = errorIfNull "aohs" $ map (fillInAdjustedHappiness b c) aos
  where rs = errorIfNull "rs" $ map P.pResponse pReport
        -- rs = just the updated responses
        aos = errorIfNull "aos" $ sumByAction rs
        -- aos = for each action, the ouputs summed term-by-term

-- | Internal method
errorIfNull :: String -> [a] -> [a]
errorIfNull desc xs = if null xs
                        then error ("null " ++ desc)
                        else xs

-- | Internal method
onlyModelsIn :: Brain ct pt p a m -> [(GSOM.Label, UI.Double)] -> Bool
onlyModelsIn b = all (GSOM.hasLabel (classifier b) . fst)

-- | Internal method
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = error "maximaBy: empty list"
maximaBy f xs = map snd . last . groupBy ((==) `on` fst)
               . sortBy (comparing fst) . map (\x -> (f x, x)) $ xs

-- | Internal method
chooseAny :: Brain ct pt p a m -> [x] -> x
chooseAny b xs = xs !! (seed `mod` n)
  where seed = fromIntegral $ tiebreaker b
        n = length xs -- the list will be short

-- | Internal method
fillInAdjustedHappiness
  :: Brain ct pt p a m -> Condition -> (a, [PM1.Double])
      -> (a, [PM1.Double], UI.Double)
fillInAdjustedHappiness b c (a, os) = (a, os, adjustedHappiness b c os)

-- | Internal method
adjustedHappiness
  :: Brain ct pt p a m -> Condition -> [PM1.Double] -> UI.Double
adjustedHappiness b c = happiness b . adjustCondition c

-- | Internal method
adjustCondition :: Condition -> [PM1.Double] -> Condition
adjustCondition c os =
  map PM1.crop $
    zipWith (+) (map UI.wide c) (map UI.wide os)

-- | Internal method
adjustActionCounts
  :: Ord a => Brain ct pt p a m -> a -> Brain ct pt p a m
adjustActionCounts b a = b { actionCounts=cs' }
  where cs = actionCounts b
        cs' = M.alter inc a cs
        inc Nothing  = Just 1
        inc (Just n) = Just (n+1)

-- | Evaluates the input patterns and the current condition.
--   Returns the classification report and the updated brain.
classifyInputs
  :: (SOM.Adjuster ct, SOM.PatternType ct ~ p,
     SOM.MetricType ct ~ UI.Double, SOM.TimeType ct ~ Word32)
  => Brain ct pt p a m -> [p] -> (Cl.ClassifierReport p, Brain ct pt p a m)
classifyInputs b ps = (cReport, b { classifier=c' })
  where (cReport, c') = Cl.classifySetAndTrain (classifier b) ps

-- | Internal method
sumByAction :: Eq a => [Response a] -> [(a, [PM1.Double])]
sumByAction rs = map sumByAction' rss
  where rss = groupBy sameAction rs
        sameAction x y = action x == action y

-- | Internal method
sumByAction' :: [Response a] -> (a, [PM1.Double])
sumByAction' [] = error "no responses to sum"
sumByAction' rs = (a, os)
  where a = action $ head rs
        os = sumTermByTerm $ map outcomes rs

-- | Internal method
sumTermByTerm :: Num a => [[a]] -> [a]
sumTermByTerm [] = []
sumTermByTerm [xs] = xs
sumTermByTerm (xs:ys:zss) = sumTermByTerm (ws:zss)
  where ws = zipWith (+) xs ys

-- | Predicts the outcomes of a set of responses based on the Predictor
--   models, and updates the outcome field in each response.
--   Some of the responses we're considering are in response to the
--   scenario we're most likely facing, and some are in response to
--   alternative scenarios we want to consider.
--   Each predicted outcome is scaled by how close it is to the
--   scenario we think we're facing.
--   So each input response is paired with the likelihood that the
--   scenario it describes is actually the one we're facing.
--   This approach allows us to weigh risks and outcomes.
--   We can compare, for example, an action with a high probability of a
--   somewhat good outcome, to an action with a low probability of a
--   really bad outcome.
--   For each response, returns the updated response (with the predicted
--   outcome filled in), the probability associated with the scenario,
--   the adjusted probability based on how well the predictor model
--   matches the proposed response, the label of the predictor model
--   that best matches the response, and the unadjusted outcomes from
--   the model.
predictAll
  :: (SOM.Adjuster pt, SOM.PatternType pt ~ Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32, Eq a)
  => Brain ct pt p a m -> [(Response a, UI.Double)] -> PredictorReport a
predictAll b = foldl' (predictOne b) []

-- | Internal method
predictOne
  :: (SOM.Adjuster pt, SOM.PatternType pt ~ Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32, Eq a)
  => Brain ct pt p a m -> PredictorReport a -> (Response a, UI.Double)
      -> PredictorReport a
predictOne b xs (r, p) = P.predict (predictor b) r p:xs

-- | Detailed information about the wain's reflection on the outcome
--   of an action.
data ReflectionReport a =
  ReflectionReport
    {
      -- | Information about what the brain learned through reflection
      brrLearningReport :: P.LearningReport a,
      -- | The error in the brain's prediction of the change to
      --   happiness.
      brrErr            :: Double
    } deriving (Generic, Show, NFData)

prettyReflectionReport :: Pretty a => ReflectionReport a -> [String]
prettyReflectionReport r =
  ("err=" ++ pretty (brrErr r)) : P.prettyLearningReport (brrLearningReport r)

-- | Considers whether the wain is happier or not as a result of the
--   last action it took, and modifies the decision models accordingly.
--   Returns the updated brain, and the error in the brain's
--   prediction of the change to happiness.
reflect
  :: (SOM.Adjuster pt, SOM.PatternType pt ~ Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32, Eq a)
  => Brain ct pt p a m -> Condition
      -> (Maybe (ReflectionReport a), Brain ct pt p a m)
reflect b cAfter
  = case (lastChoice b) of
      Just (r, cBefore)
        -> (Just report', b { predictor=d' })
          where osActual = map UI.narrow $ zipWith (-) (map UI.wide cAfter)
                  (map UI.wide cBefore)
                rReflect = r {outcomes = osActual}
                (lReport, d') = P.learn (predictor b) rReflect
                osPredicted = outcomes r
                cPredicted = adjustCondition cBefore osPredicted
                deltaH = UI.wide (happiness b cAfter)
                           - UI.wide (happiness b cBefore)
                predictedDeltaH = PM1.wide (happiness b cPredicted)
                           - PM1.wide (happiness b cBefore)
                report' = ReflectionReport
                           {
                             brrLearningReport = lReport,
                             brrErr = abs (deltaH - predictedDeltaH)
                           }
      Nothing -> (Nothing, b)

-- | Teaches the brain a set of patterns, and a label for each of them.
imprintStimulus
  :: (SOM.Adjuster ct, SOM.PatternType ct ~ p,
     SOM.MetricType ct ~ UI.Double, SOM.TimeType ct ~ Word32)
  => Brain ct pt p a m -> [(GSOM.Label, p)]
  -> ([GSOM.ImprintDetail p], Brain ct pt p a m)
imprintStimulus b lps = (iReport, b { classifier=d })
  where (iReport, d) = Cl.imprintSet (classifier b) lps

-- | Teaches the brain a desirable action to take in response to a
--   stimulus.
--   This can be used to help children learn by observing their parents.
--   It can also be used to allow wains to learn from others.
--   Returns:
--
--   * a detailed report of the imprint process
--   * the updated brain.
imprintResponse
  :: (SOM.Adjuster pt, SOM.PatternType pt ~ Response a,
     SOM.MetricType pt ~ UI.Double, SOM.TimeType pt ~ Word32, Eq a)
  => Brain ct pt p a m -> [GSOM.Label] -> a
  -> (P.LearningReport a, Brain ct pt p a m)
imprintResponse b ls a = (irReport, b { predictor=d2 })
  where d = predictor b
        (irReport, d2) = P.imprintOrReinforce d ls a os deltas
        os = imprintOutcomes b
        deltas = reinforcementDeltas b

-- | Evaluates a condition and reports the resulting happiness.
happiness :: Brain ct pt p a m -> Condition -> UI.Double
happiness b = weightedSum (happinessWeights b)

-- | A metric for how flexible a brain is at making decisions.
decisionQuality :: Brain ct pt p a m -> Int
decisionQuality = GSOM.discrimination . M.elems . actionCounts

-- | Eliminate any responses that refer to obsolete classifier models.
pruneObsoleteResponses
  :: (SOM.Adjuster pt, Eq a, Pretty m) => Brain ct pt p a m -> Brain ct pt p a m
pruneObsoleteResponses b = b { predictor=prune $ predictor b }
  where prune = P.filterLabels ls
        ls = Cl.labels $ classifier b

instance (SOM.Adjuster ct, Report ct, SOM.Adjuster pt, Report pt,
          Pretty p, Pretty a, Eq a, Pretty m)
  => Report (Brain ct pt p a m) where
  report b = [ "muser: " ++ pretty (muser b),
               "DSQ: " ++ pretty (decisionQuality b),
               "happiness weights: " ++ pretty (happinessWeights b),
               "strictness: " ++ pretty (strictness b),
               "imprint outcomes: " ++ pretty (imprintOutcomes b),
               "reinforcement deltas: " ++ pretty (reinforcementDeltas b),
               "action counts: " ++ pretty (M.elems $ actionCounts b) ]
             ++ map ("classifier " ++) (report (classifier b))
             ++ map ("predictor " ++) (report (predictor b))
