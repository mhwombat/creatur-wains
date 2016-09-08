-----------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.BrainInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2016
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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.BrainInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic, put, get)
import ALife.Creatur.Genetics.Diploid (Diploid)
import qualified ALife.Creatur.Wain.Classifier as Cl
import qualified ALife.Creatur.Wain.Predictor as P
import qualified ALife.Creatur.Wain.GeneticSOM as GSOM
import ALife.Creatur.Wain.Response (Response(..), action, outcomes)
import ALife.Creatur.Wain.Statistics (Statistical, Statistic, stats,
  prefix, iStat, dStat, dStats)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, doubleToPM1,
  pm1ToDouble)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble,
  forceDoubleToUI)
import ALife.Creatur.Wain.Weights (Weights, weightAt, weightedSum,
  numWeights, toUIDoubles)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Function (on)
import Data.List (groupBy, sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)
-- import Text.Printf (printf)

-- | A wain's condition
type Condition = [UIDouble]

-- | A brain which recommends reponses to stimuli, and learns from the
--   outcomes.
data Brain p ct pt a = Brain
  {
    -- | Component that categorises and identifies patterns
    _classifier :: Cl.Classifier p ct,
    -- | Component that predicts the outcome of possible actions
    _predictor :: P.Predictor a pt,
    -- | Weights for evaluating happiness
    _happinessWeights :: Weights,
    -- | Used to break ties when actions seem equally promising
    _tiebreaker :: Word8,
    -- | Used to determine how similar a response model is to the
    --   response we're considering.
    _responseWeights :: Weights,
    -- If the difference between a predictor model and a response we're
    -- considering exceeds this, we won't use it to predict the
    -- the outcome of an action.
    _decisionDiffThreshold :: UIDouble,
    -- | If a wain has no model for a response it's considering, it
    --   will use these values as a prediction.
    --   Positive values make the wain optimistic and more likely to
    --   take risks. A negative value makes the wain pessimistic and
    --   risk-averse.
    _defaultOutcomes :: [PM1Double],
    -- | When a wain observes a response that it has never seen before,
    --   it will assume the action has the following outcomes.
    --   Normally these values should all be positive.
    _imprintOutcomes :: [PM1Double],
    -- | When a wain observes a response that it already knows, it will
    --   reinforce it by re-learning the model augmented with these
    --   delta outcomes.
    --   Normally these values should all be positive.
    _reinforcementDeltas :: [PM1Double],
    -- | Number of times each action has been used
    _actionCounts :: M.Map a Int
  } deriving (Generic, Eq, NFData)
makeLenses ''Brain

-- | @'makeBrain' c m p hw t ios rds@ returns a brain with
--   classifier @c@, muser @m@, predictor @p@, happiness weights @hw@,
--   tiebreaker @t@, imprint outcomes @ios@, and reinforcement deltas
--   @rds@. See @Brain@ for an explanation of these parameters.
makeBrain
  :: Cl.Classifier p ct -> P.Predictor a pt -> Weights -> Word8
     -> Weights -> UIDouble -> [PM1Double] -> [PM1Double] -> [PM1Double]
     -> Either [String] (Brain p ct pt a)
makeBrain c p hw t rw ddt dos ios rds
  | numWeights hw /= 4
      = Left ["incorrect number of happiness weights"]
  -- TODO Check number of response weights = #objects + 1
  | numWeights rw < 2
      = Left ["incorrect number of response weights"]
  | length dos /= 4
      = Left ["incorrect number of default outcomes"]
  | length ios /= 4
      = Left ["incorrect number of imprint outcomes"]
  | length rds /= 4
      = Left ["incorrect number of reinforcement deltas"]
  | otherwise
      = Right $ Brain c p hw t rw ddt dos ios rds M.empty

instance (Serialize p, Serialize ct, Serialize pt, Serialize a, Eq a,
  Ord a, GSOM.Tweaker ct, GSOM.Tweaker pt, p ~ GSOM.Pattern ct,
  Response a ~ GSOM.Pattern pt)
    => Serialize (Brain p ct pt a)

instance (Diploid p, Diploid ct, Diploid pt, Diploid a, Eq a, Ord a,
  GSOM.Tweaker ct, GSOM.Tweaker pt, p ~ GSOM.Pattern ct,
  Response a ~ GSOM.Pattern pt)
    => Diploid (Brain p ct pt a)

instance (Eq a, Ord a)
      => Statistical (Brain p ct pt a) where
  stats b@(Brain c p hw t rw ddt dos ios rds _)
    | length dos < 4 = error "dos not long enough"
    | length ios < 4 = error "ios not long enough"
    | length rds < 4 = error "rds not long enough"
    | otherwise =
      map (prefix "classifier ") (stats c)
      ++ map (prefix "predictor ") (stats p)
      ++ [ iStat "DQ" $ decisionQuality b,
           dStat "energyWeight" . uiToDouble $ hw `weightAt` 0,
           dStat "passionWeight" . uiToDouble $ hw `weightAt` 1,
           dStat "boredomWeight" . uiToDouble $ hw `weightAt` 2,
           dStat "litterSizeWeight" . uiToDouble $ hw `weightAt` 3,
           dStat "decisionDiffThreshold" ddt,
           dStat "energyDefault" . pm1ToDouble $ dos !! 0,
           dStat "passionDefault" . pm1ToDouble $ dos !! 1,
           dStat "boredomDefault" . pm1ToDouble $ dos !! 2,
           dStat "litterSizeDefault" . pm1ToDouble $ dos !! 3,
           dStat "energyImprint" . pm1ToDouble $ ios !! 0,
           dStat "passionImprint" . pm1ToDouble $ ios !! 1,
           dStat "boredomImprint" . pm1ToDouble $ ios !! 2,
           dStat "litterSizeImprint" . pm1ToDouble $ ios !! 3,
           dStat "energyReinforcement" . pm1ToDouble $ rds !! 0,
           dStat "passionReinforcement" . pm1ToDouble $ rds !! 1,
           dStat "boredomReinforcement" . pm1ToDouble $ rds !! 2,
           dStat "litterSizeReinforcement" . pm1ToDouble $ rds !! 3,
           iStat "tiebreaker" t]
      ++ responseWeightStats rw

responseWeightStats :: Weights -> [Statistic]
responseWeightStats ws
  = dStat "actionWeight" (head xs)
    : dStats "labelWeight" (tail xs)
  where xs = map uiToDouble $ toUIDoubles ws

instance (Show p, Show a, Show ct, Show pt, Eq a)
      => Show (Brain p ct pt a) where
  show (Brain c p hw t rw ddt dos ios rds ks) = "Brain (" ++ show c ++ ") ("
    ++ show p ++ ") (" ++ show hw ++ ") "
    ++ show t ++ " (" ++ show rw ++ ") " ++ show ddt ++ show dos ++ " "
    ++ show ios ++ show rds ++ " (" ++ show ks ++ ")"

instance (Genetic p, Genetic ct, Genetic pt, Genetic a, Eq a, Ord a,
          GSOM.Tweaker ct, GSOM.Tweaker pt, p ~ GSOM.Pattern ct,
          Response a ~ GSOM.Pattern pt)
    => Genetic (Brain p ct pt a) where
    put (Brain c p hw t rw ddt dos ios rds _)
      = put c >> put p >> put hw >> put t >> put rw >> put ddt
          >> put dos >> put ios >> put rds
    get = do
      c <- get
      p <- get
      hw <- get
      t <- get
      rw <- get
      ddt <- get
      dos <- get
      ios <- get
      rds <- get
      -- Use the safe constructor!
      case (makeBrain <$> c <*> p <*> hw <*> t <*> rw <*> ddt <*> dos
             <*> ios <*> rds) of
        Left msgs -> return $ Left msgs
        Right b   -> return b

-- | Chooses a response based on the stimuli (input patterns) and
--   the wain's condition.
--   Returns:
--
--   * for each object in the stimuli, the difference between that
--     object and each classifier model paired with the model's label
--   * the hypotheses considered, together with the probability that
--     each hypothesis is true
--   * the responses considered (with predicted outcomes filled in)
--     together with the estimated probability that each response
--     is based on a correct hypothesis, the label of the predictor
--     model upon which the response is based, and the expected
--     condition after the response
--   * the actions considered, paired with the expected condition after
--     the action
--   * the chosen response
--   * the updated brain
--
--   NOTE: The response chosen might be a response modelled on
--   a different scenario than the one we think we're in.
--   This might happen, for example, if the ideal response to the
--   most likely scenario has a somewhat good outcome, but the ideal
--   response to a somewhat likely alternative scenario has a really
--   bad outcome. "I think that food is edible, but I'm not going to
--   eat it just in case I've misidentified it and it's poisonous."
chooseAction
  :: (Eq a, Enum a, Bounded a, Ord a)
      => (a -> a -> UIDouble) -> Brain p ct pt a -> [p] -> Condition
        -> ( [[(Cl.Label, GSOM.Difference)]],
             [([(Response a, Cl.Difference, UIDouble, [PM1Double])],
               Response a, UIDouble)],
             Response a,
             Brain p ct pt a)
chooseAction actionDiff b ps c = (ldss, xrhs, r, b3)
  where (ls, ldss, b2) = classifyInputs b ps
        as = [minBound ..maxBound]
        (xss, rs) = unzip $ map (evaluateResponse actionDiff b ls ldss) as
        rhs = errorIfNull "rs" $ map (fillInAdjustedHappiness b2 c) rs
        hs = map snd rhs
        xrhs = zip3 xss rs hs
        (r, _) = chooseAny b . maximaBy snd $ rhs
        b3 = adjustActionCounts b2 r

evaluateResponse
  :: (Bounded a, Enum a, Ord a)
    => (a -> a -> GSOM.Difference)
      -> Brain p ct t a -> [GSOM.Label]
        -> [[(GSOM.Label, GSOM.Difference)]] -> a
          -> ([(Response a, GSOM.Difference, UIDouble, [PM1Double])],
                            Response a)
evaluateResponse actionDiff b ls ldss a = (xs, Response ls a os)
  where ws = _responseWeights b
        dos = _defaultOutcomes b
        p = _predictor b
        ddt = _decisionDiffThreshold b
        (xs, os) = P.evaluateResponse ddt ws actionDiff dos p ldss a

decisionReport
  :: Show a
    => ([(Response a, GSOM.Difference, UIDouble, [PM1Double])],
         Response a, UIDouble) -> [String]
decisionReport (xs, r, h) = msg:msgs
  where msg = "Predicted output of action " ++ show a ++ " is "
                ++ show os ++ " with happiness " ++ show h
                ++ " based on the following models"
        msgs = P.decisionReport xs
        a = view action r
        os = view outcomes r

-- | Evaluates the input patterns and the current condition.
--   Returns the "signature" (differences between the input pattern
--   and each model in the classifier) of each input pattern,
--   and the updated brain.
classifyInputs
  :: Brain p ct pt a -> [p]
    -> ([Cl.Label], [[(Cl.Label, GSOM.Difference)]],
        Brain p ct pt a)
classifyInputs b ps = (bmus, ldss, b')
  where (bmus, ldss, c')
          = Cl.classifySetAndTrain (_classifier b) ps
        b' = set classifier c' b

-- | Internal method
errorIfNull :: String -> [a] -> [a]
errorIfNull desc xs = if null xs
                        then error ("null " ++ desc)
                        else xs

-- | Internal method
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = error "maximaBy: empty list"
maximaBy f xs = map snd . head . reverse . groupBy ((==) `on` fst)
               . sortBy (comparing fst) . map (\x -> (f x, x)) $ xs

-- | Internal method
chooseAny :: Brain p ct pt a -> [x] -> x
chooseAny b xs = xs !! (seed `mod` n)
  where seed = fromIntegral $ _tiebreaker b
        n = length xs -- the list will be short

-- | Internal method
fillInAdjustedHappiness
  :: Brain p ct pt a -> Condition -> Response a
    -> (Response a, UIDouble)
fillInAdjustedHappiness b c r = (r, adjustedHappiness b c r)

-- | Internal method
adjustedHappiness
  :: Brain p ct pt a -> Condition -> Response a -> UIDouble
adjustedHappiness b c r
  = happiness b $ adjustCondition c (view outcomes r)

-- | Internal method
adjustCondition :: Condition -> [PM1Double] -> Condition
adjustCondition c os =
  map forceDoubleToUI $
    zipWith (+) (map uiToDouble c) (map pm1ToDouble os)

-- | Internal method
adjustActionCounts
  :: Ord a => Brain p ct pt a -> Response a -> Brain p ct pt a
adjustActionCounts b r = set actionCounts cs' b
  where a = _action r
        cs = _actionCounts b
        cs' = M.alter inc a cs
        inc Nothing = Just 1
        inc (Just n) = Just (n+1)

-- | Considers whether the wain is happier or not as a result of the
--   last action it took, and modifies the decision models accordingly.
--   Returns the updated brain, and the error in the brain's
--   prediction of the outcome.
reflect
  :: Eq a
    => Brain p ct pt a -> Response a -> Condition -> Condition
      -> (Brain p ct pt a, Double)
reflect b r cBefore cAfter = (set predictor d' b, err)
  where osActual = map doubleToPM1 $ zipWith (-) (map uiToDouble cAfter)
          (map uiToDouble cBefore)
        d' = GSOM.train (_predictor b) (r {_outcomes = osActual})
        osPredicted = _outcomes r
        cPredicted = adjustCondition cBefore osPredicted
        deltaH = uiToDouble (happiness b cAfter)
                   - uiToDouble (happiness b cBefore)
        predictedDeltaH = uiToDouble (happiness b cPredicted)
                   - uiToDouble (happiness b cBefore)
        err = abs (deltaH - predictedDeltaH)

-- | Teaches the brain a desirable action to take in response to a
--   stimulus.
--   This can be used to help children learn by observing their parents.
--   It can also be used to allow wains to learn from others.
--   Returns the classifier labels paired with the difference of
--   the associated model and the input patterns,
--   the scenarios that were considered paired with their estimated
--   probability, the label of the new or adjusted predictor model,
--   the new or adjusted predictor model itself,
--   and the updated brain.
imprint
  :: Eq a
    => Brain p ct pt a -> [p] -> a
      -> ([[(Cl.Label, GSOM.Difference)]],
            P.Label, Response a, Brain p ct pt a)
imprint b ps a = (lds, bmu, r, b3)
  where (ls, lds, b2) = classifyInputs b ps
        (bmu, r, b3) = imprintPredictor b2 ls a

-- | Internal method
imprintPredictor
  :: Eq a
    => Brain p ct pt a -> [GSOM.Label] -> a
      -> (P.Label, Response a, Brain p ct pt a)
imprintPredictor b ls a = (bmu, r, set predictor d2 b)
  where d = _predictor b
        (bmu, r, d2) = P.imprintOrReinforce d ls a os deltas
        os = _imprintOutcomes b
        deltas = _reinforcementDeltas b

-- | Evaluates a condition and reports the resulting happiness.
happiness :: Brain p ct pt a -> Condition -> UIDouble
happiness b = weightedSum (_happinessWeights b)

-- | A metric for how flexible a brain is at making decisions.
decisionQuality :: Brain p ct pt a -> Int
decisionQuality = GSOM.discrimination . M.elems . _actionCounts
