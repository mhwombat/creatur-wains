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
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module ALife.Creatur.Wain.BrainInternal where

import           ALife.Creatur.Genetics.BRGCWord8 (Genetic, get, put)
import           ALife.Creatur.Genetics.Diploid   (Diploid)
import qualified ALife.Creatur.Wain.Classifier    as Cl
import qualified ALife.Creatur.Wain.GeneticSOM    as GSOM
import qualified ALife.Creatur.Wain.Muser         as M
import           ALife.Creatur.Gene.Numeric.PlusMinusOne
    (PM1Double, doubleToPM1, pm1ToDouble)
import qualified ALife.Creatur.Wain.Predictor     as P
import           ALife.Creatur.Wain.Pretty        (Pretty, pretty)
import           ALife.Creatur.Wain.Probability
    (Probability, hypothesise, prettyProbability)
import           ALife.Creatur.Wain.Report        (Report, report)
import           ALife.Creatur.Wain.Response      (Response (..))
import           ALife.Creatur.Wain.Statistics
    (Statistical, dStat, iStat, prefix, stats)
import           ALife.Creatur.Gene.Numeric.UnitInterval
    (UIDouble, forceDoubleToUI, uiToDouble)
import           ALife.Creatur.Gene.Numeric.Weights
    (Weights, numWeights, weightAt, weightedSum)
import           Control.DeepSeq                  (NFData)
import           Control.Lens
import           Data.Function                    (on)
import           Data.List
    (foldl', groupBy, sortBy)
import qualified Data.Map.Strict                  as M
import           Data.Ord                         (comparing)
import           Data.Serialize                   (Serialize)
import           Data.Word                        (Word64, Word8)
import           GHC.Generics                     (Generic)
import           Text.Printf                      (printf)

-- | A wain's condition
type Condition = [UIDouble]

-- | A brain which recommends reponses to stimuli, and learns from the
--   outcomes.
data Brain p ct pt m a = Brain
  {
    -- | Component that categorises and identifies patterns
    _classifier          :: Cl.Classifier p ct,
    -- | Component that generates response models for consideration
    _muser               :: m,
    -- | Component that decides what actions to take
    _predictor           :: P.Predictor a pt,
    -- | Weights for evaluating happiness
    _happinessWeights    :: Weights,
    -- | Used to break ties when actions seem equally promising
    _tiebreaker          :: Word8,
    -- | Controls how willing the wain is to consider alternative
    --   classifications when making decisions.
    --   Must be >= 1.
    _strictness          :: Word64,
    -- | When a wain observes a response that it has never seen before,
    --   it will assume the action has the following outcomes.
    --   Normally these values should all be positive.
    _imprintOutcomes     :: [PM1Double],
    -- | When a wain observes a response that it already knows, it will
    --   reinforce it by re-learning the model augmented with these
    --   delta outcomes.
    --   Normally these values should all be positive.
    _reinforcementDeltas :: [PM1Double],
    -- | Number of times each action has been used
    _actionCounts        :: M.Map a Int
  } deriving (Generic, Eq, NFData)
makeLenses ''Brain

-- | @'makeBrain' c m p hw t ios rds@ returns a brain with
--   classifier @c@, muser @m@, predictor @p@, happiness weights @hw@,
--   tiebreaker @t@, imprint outcomes @ios@, and reinforcement deltas
--   @rds@. See @Brain@ for an explanation of these parameters.
makeBrain
  :: M.Muser m
     => Cl.Classifier p ct -> m -> P.Predictor a pt -> Weights -> Word8
       -> Word64 -> [PM1Double] -> [PM1Double]
       -> Either [String] (Brain p ct pt m a)
makeBrain c m p hw t x ios rds
  | x < 1
      = Left ["strictness < 1"]
  | numWeights hw /= 4
      = Left ["incorrect number of happiness weights"]
  | length (M.defaultOutcomes m) /= 4
      = Left ["incorrect number of default outcomes"]
  | length ios /= 4
      = Left ["incorrect number of imprint outcomes"]
  | length rds /= 4
      = Left ["incorrect number of reinforcement deltas"]
  | otherwise
      = Right $ Brain c m p hw t x ios rds M.empty

instance (Serialize p, Serialize ct, Serialize pt, Serialize a,
  Serialize m, Eq a, Ord a, GSOM.Tweaker ct, GSOM.Tweaker pt,
  p ~ GSOM.Pattern ct, Response a ~ GSOM.Pattern pt)
    => Serialize (Brain p ct pt m a)

instance (Diploid p, Diploid ct, Diploid pt, Diploid a, Diploid m,
  Eq a, Ord a, GSOM.Tweaker ct, GSOM.Tweaker pt, p ~ GSOM.Pattern ct,
  Response a ~ GSOM.Pattern pt)
    => Diploid (Brain p ct pt m a)

instance (Eq a, Ord a,
          Statistical [(GSOM.Label, p)], Statistical ct,
          Statistical pt, Statistical m,
          Statistical [(GSOM.Label, Response a)])
    => Statistical (Brain p ct pt m a) where
  stats b@(Brain c m p hw t s ios rds _)
    | length ios < 4 = error "ios not long enough"
    | length rds < 4 = error "rds not long enough"
    | otherwise =
      map (prefix "classifier ") (stats c)
        ++ stats m
        ++ map (prefix "predictor ") (stats p)
        ++ [ iStat "DQ" $ decisionQuality b,
             dStat "energyWeight" . uiToDouble $ hw `weightAt` 0,
             dStat "passionWeight" . uiToDouble $ hw `weightAt` 1,
             dStat "boredomWeight" . uiToDouble $ hw `weightAt` 2,
             dStat "litterSizeWeight" . uiToDouble $ hw `weightAt` 3,
             dStat "energyImprint" . pm1ToDouble $ head ios,
             dStat "passionImprint" . pm1ToDouble $ ios !! 1,
             dStat "boredomImprint" . pm1ToDouble $ ios !! 2,
             dStat "litterSizeImprint" . pm1ToDouble $ ios !! 3,
             dStat "energyReinforcement" . pm1ToDouble $ head rds,
             dStat "passionReinforcement" . pm1ToDouble $ rds !! 1,
             dStat "boredomReinforcement" . pm1ToDouble $ rds !! 2,
             dStat "litterSizeReinforcement" . pm1ToDouble $ rds !! 3,
             iStat "tiebreaker" t, iStat "strictness" s ]

instance (Show p, Show a, Show ct, Show pt, Show m, Eq a)
      => Show (Brain p ct pt m a) where
  show (Brain c m p hw t s ios rds ks) = "Brain (" ++ show c ++ ") ("
    ++ show m ++ ") (" ++ show p ++ ") (" ++ show hw ++ ") "
    ++ show t ++ " " ++ show s ++ " " ++ show ios ++ show rds
    ++ " (" ++ show ks ++ ")"

instance (Genetic p, Genetic ct, Genetic pt, Genetic m, Genetic a,
          M.Muser m, Eq a, Ord a, GSOM.Tweaker ct, GSOM.Tweaker pt,
          p ~ GSOM.Pattern ct, Response a ~ GSOM.Pattern pt)
    => Genetic (Brain p ct pt m a) where
    put (Brain c m p hw t s ios rds _)
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

-- | A set of hypotheses about the scenario the wain is
--   facing, paired with the estimated probability that each
--   hypothesis is true.
--   (A hypothesis is a set of classifier labels.)
type ScenarioReport = [([Cl.Label], Probability)]

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
type ActionReport a = [(a, [PM1Double], UIDouble)]

-- | Generates a human readable summary of a decision.
prettyActionReport :: Pretty a => ActionReport a -> [String]
prettyActionReport = map f
  where f (a, os, h) = "predicted outcomes of " ++ pretty a ++ " are "
          ++ unwords (map (printf "%.3f" . pm1ToDouble) os)
          ++ " with resulting happiness "
          ++ printf "%.3f" (uiToDouble h)

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
  :: (M.Muser m, Eq a, Ord a, GSOM.Tweaker pt,
    M.Action m ~ a, Response a ~ GSOM.Pattern pt)
      => Brain p ct pt m a
        -> [p]
        -> Condition
        -> (DecisionReport p a, Brain p ct pt m a)
chooseAction b ps c = (dReport, b4)
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
        dReport = DecisionReport
                   {
                     bdrStimulus = ps,
                     bdrClassifierReport = cReport,
                     bdrScenarioReport = sReport,
                     bdrPredictorReport = pReport,
                     bdrActionReport = aReport,
                     bdrRecommendedResponse = Response cBmus a os
                   }

-- | Internal method
generateScenarios
  :: Brain p ct pt m a -> Cl.ClassifierReport p -> ScenarioReport
-- generateScenarios b cReport = if null sps'
--                     then sps -- nothing to base estimate on; just guess
--                     else sps'
generateScenarios b cReport = sps
  where ldss = Cl.diffs cReport
        -- ldss = the SGM labels paired with the difference between the
        --       inputs and the corresponding model
        sps = errorIfNull "sps" $ hypothesise (_strictness b) ldss
        --   sps = set of hypotheses about the scenario the wain is
        --   facing, paired with the estimated probability that each
        --   hypothesis is true. (A hypothesis is a set of labels.)
--         sps' = filter (P.hasScenario (_predictor b) . fst) sps

-- | Internal method
generateResponses
  :: (M.Muser m, Eq a, GSOM.Tweaker pt, a ~ M.Action m,
     Response a ~ GSOM.Pattern pt)
  => Brain p ct pt m a -> ScenarioReport -> PredictorReport a
generateResponses b sReport = errorIfNull "pReport" $ predictAll b rps
  where as = P.actions $ _predictor b
        -- as = list of actions to evaluate
        rps = errorIfNull "rps" $ M.generateResponses (_muser b) as sReport
        -- rps = list of responses to consider, paired with the
        --       probability that the response is based on the correct
        --       scenario.


-- | Internal method
evaluateActions
  :: Eq a
  => Brain p ct pt m a
  -> Condition
  -> PredictorReport a
  -> ActionReport a
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
onlyModelsIn :: Brain p ct pt m a -> [(Cl.Label, GSOM.Difference)] -> Bool
onlyModelsIn b = all (GSOM.hasLabel (_classifier b) . fst)

-- | Internal method
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = error "maximaBy: empty list"
maximaBy f xs = map snd . last . groupBy ((==) `on` fst)
               . sortBy (comparing fst) . map (\x -> (f x, x)) $ xs

-- | Internal method
chooseAny :: Brain p ct pt m a -> [x] -> x
chooseAny b xs = xs !! (seed `mod` n)
  where seed = fromIntegral $ _tiebreaker b
        n = length xs -- the list will be short

-- | Internal method
fillInAdjustedHappiness
  :: Brain p ct pt m a -> Condition -> (a, [PM1Double])
    -> (a, [PM1Double], UIDouble)
fillInAdjustedHappiness b c (a, os) = (a, os, adjustedHappiness b c os)

-- | Internal method
adjustedHappiness
  :: Brain p ct pt m a -> Condition -> [PM1Double] -> UIDouble
adjustedHappiness b c = happiness b . adjustCondition c

-- | Internal method
adjustCondition :: Condition -> [PM1Double] -> Condition
adjustCondition c os =
  map forceDoubleToUI $
    zipWith (+) (map uiToDouble c) (map pm1ToDouble os)

-- | Internal method
adjustActionCounts
  :: Ord a => Brain p ct pt m a -> a -> Brain p ct pt m a
adjustActionCounts b a = set actionCounts cs' b
  where cs = _actionCounts b
        cs' = M.alter inc a cs
        inc Nothing  = Just 1
        inc (Just n) = Just (n+1)

-- | Evaluates the input patterns and the current condition.
--   Returns the classification report and the updated brain.
classifyInputs
  :: Brain p ct pt m a
    -> [p]
    -> (Cl.ClassifierReport p, Brain p ct pt m a)
classifyInputs b ps = (cReport, b')
  where (cReport, c') = Cl.classifySetAndTrain (_classifier b) ps
        b' = set classifier c' b

-- | Internal method
sumByAction :: Eq a => [Response a] -> [(a, [PM1Double])]
sumByAction rs = map sumByAction' rss
  where rss = groupBy sameAction rs
        sameAction x y = _action x == _action y

-- | Internal method
sumByAction' :: [Response a] -> (a, [PM1Double])
sumByAction' [] = error "no responses to sum"
sumByAction' rs = (a, os)
  where a = _action $ head rs
        os = sumTermByTerm $ map _outcomes rs

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
  :: (Eq a, GSOM.Tweaker pt, Response a ~ GSOM.Pattern pt)
    => Brain p ct pt m a -> [(Response a, Probability)]
      -> PredictorReport a
predictAll b = foldl' (predictOne b) []

-- | Internal method
predictOne
  :: (Eq a, GSOM.Tweaker pt, Response a ~ GSOM.Pattern pt)
    => Brain p ct pt m a
      -> PredictorReport a
        -> (Response a, Probability)
          -> PredictorReport a
predictOne b xs (r, p) = P.predict (_predictor b) r p:xs

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
  :: Eq a
    => Brain p ct pt m a -> Response a -> Condition -> Condition
      -> (ReflectionReport a, Brain p ct pt m a)
reflect b r cBefore cAfter = (report', set predictor d' b)
  where osActual = map doubleToPM1 $ zipWith (-) (map uiToDouble cAfter)
          (map uiToDouble cBefore)
        rReflect = r {_outcomes = osActual}
        (lReport, d') = P.learn (_predictor b) rReflect
        osPredicted = _outcomes r
        cPredicted = adjustCondition cBefore osPredicted
        deltaH = uiToDouble (happiness b cAfter)
                   - uiToDouble (happiness b cBefore)
        predictedDeltaH = uiToDouble (happiness b cPredicted)
                   - uiToDouble (happiness b cBefore)
        report' = ReflectionReport
                   {
                     brrLearningReport = lReport,
                     brrErr = abs (deltaH - predictedDeltaH)
                   }

-- | Teaches the brain a set of patterns, and a label for each of them.
imprintStimulus
  :: Brain p ct pt m a
  -> [(Cl.Label, p)]
  -> ([GSOM.ImprintDetail p], Brain p ct pt m a)
imprintStimulus b lps = (iReport, set classifier d b)
  where (iReport, d) = Cl.imprintSet (_classifier b) lps

-- | Teaches the brain a desirable action to take in response to a
--   stimulus.
--   This can be used to help children learn by observing their parents.
--   It can also be used to allow wains to learn from others.
--   Returns:
--
--   * a detailed report of the imprint process
--   * the updated brain.
imprintResponse
  :: Eq a
    => Brain p ct pt m a
      -> [P.Label]
      -> a
      -> (P.LearningReport a, Brain p ct pt m a)
imprintResponse b ls a = (irReport, set predictor d2 b)
  where d = _predictor b
        (irReport, d2) = P.imprintOrReinforce d ls a os deltas
        os = _imprintOutcomes b
        deltas = _reinforcementDeltas b

-- | Evaluates a condition and reports the resulting happiness.
happiness :: Brain p ct pt m a -> Condition -> UIDouble
happiness b = weightedSum (_happinessWeights b)

-- | A metric for how flexible a brain is at making decisions.
decisionQuality :: Brain p ct pt m a -> Int
decisionQuality = GSOM.discrimination . M.elems . _actionCounts

-- | Eliminate any responses that refer to obsolete classifier models.
pruneObsoleteResponses :: Brain p ct pt m a -> Brain p ct pt m a
pruneObsoleteResponses b = over predictor prune b
  where prune = P.filterLabels ls
        ls = Cl.labels $ view classifier b

instance (Pretty p, Pretty ct, Pretty pt, Pretty m, Pretty a)
  => Report (Brain p ct pt m a) where
  report b = [ "muser: " ++ pretty (_muser b),
               "DSQ: " ++ pretty (decisionQuality b),
               "happiness weights: " ++ pretty (_happinessWeights b),
               "strictness: " ++ pretty (_strictness b),
               "imprint outcomes: " ++ pretty (_imprintOutcomes b),
               "reinforcement deltas: " ++ pretty (_reinforcementDeltas b),
               "action counts: " ++ pretty (M.elems $ _actionCounts b) ]
             ++ map ("classifier " ++) (report (_classifier b))
             ++ map ("predictor " ++) (report (_predictor b))
