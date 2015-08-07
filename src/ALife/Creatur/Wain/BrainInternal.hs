-----------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.BrainInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2015
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
import ALife.Creatur.Wain.Muser (Muser, generateResponses, muserOK)
import qualified ALife.Creatur.Wain.GeneticSOM as GSOM
import ALife.Creatur.Wain.Response (Response(..))
import ALife.Creatur.Wain.Scenario (Condition, Scenario(..))
import ALife.Creatur.Wain.Statistician (Probability, hypothesise)
import ALife.Creatur.Wain.Statistics (Statistical, stats, prefix,
  iStat, dStat)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, doubleToPM1,
  pm1ToDouble)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble)
import ALife.Creatur.Wain.Weights (Weights, weightAt, weightedSum)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.List (maximumBy, groupBy, foldl')
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Text.Printf (printf)

data Brain p t a = Brain
  {
    -- | Component that categorises and identifies patterns
    _classifier :: Cl.Classifier p t,
    -- | Component that generates response models for consideration
    _muser :: Muser,
    -- | Component that decides what actions to take
    _predictor :: P.Predictor a,
    -- | Weights for evaluating happiness
    _happinessWeights :: Weights,
    -- | Number of times each action has been used
    _actionCounts :: M.Map a Int
  } deriving (Generic, Eq, NFData)
makeLenses ''Brain

makeBrain
  :: Cl.Classifier p t -> Muser -> P.Predictor a -> Weights
    -> Brain p t a
makeBrain c m p hw = Brain c m p hw M.empty

instance (Serialize p, Serialize t, Serialize a, Eq a, Ord a,
  GSOM.Tweaker t, p ~ GSOM.Pattern t)
    => Serialize (Brain p t a)

instance (Diploid p, Diploid t, Diploid a, Eq a, Ord a, GSOM.Tweaker t,
  p ~ GSOM.Pattern t)
    => Diploid (Brain p t a)

instance (Eq a, Ord a)
      => Statistical (Brain p t a) where
  stats b@(Brain c m p hw _) = map (prefix "classifier ") (stats c)
    ++ (stats m)
    ++ map (prefix "predictor ") (stats p)
    ++ [ iStat "DQ" $ decisionQuality b,
         dStat "energyWeight" . uiToDouble $ hw `weightAt` 0,
         dStat "passionWeight" . uiToDouble $ hw `weightAt` 1,
         dStat "litterSizeWeight" . uiToDouble $ hw `weightAt` 2]

instance (Show p, Show a, Show t, Eq a)
      => Show (Brain p t a) where
  show (Brain c m p hw ks) = "Brain (" ++ show c ++ ") ("
    ++ show m ++ ") (" ++ show p ++ ") (" ++ show hw ++ ") ("
    ++ show ks ++ ")"

instance (Genetic p, Genetic t, Genetic a, Eq a, Ord a, GSOM.Tweaker t,
          p ~ GSOM.Pattern t)
    => Genetic (Brain p t a) where
    put (Brain c m p hw _) = put c >> put m >> put p >> put hw
    get = do
      c <- get
      m <- get
      p <- get
      hw <- get
      return $ Brain <$> c <*> m <*> p  <*> hw <*> pure M.empty


-- | Chooses a response based on the stimuli (input patterns) and
--   the wain's condition.
--   Returns the classifier labels assigned to each input pattern,
--   the classifier signature of each input pattern,
--   the predictor model on which the response is based,
--   the responses it considered (with outcome predictions filled in,
--   and paired with predictor model labels),
--   the chosen response, and the updated brain.
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
      => Brain p t a -> [p] -> Condition
        -> ([[(Cl.Label, GSOM.Difference)]],
            [(Scenario, Probability)],
            [(Response a, Probability, P.Label, PM1Double)],
            [(a, PM1Double)],
            Response a, Brain p t a)
chooseAction b ps c = (lds, sps, rplos, aos, r, b4)
  where (cBMUs, lds, b2) = classifyInputs b ps
        sps = generateScenarios c lds
        rps = generateResponses (_muser b2) sps
        (rplos, b3) = predictAll b2 rps
        rs = map (\(r1, _, _, _) -> r1) rplos
        aos = sumByAction rs
        (a, o) = maximumBy (comparing snd) aos
        b4 = adjustActionCounts b3 r
        r = Response s a o
        s = Scenario cBMUs c

adjustActionCounts :: Ord a => Brain p t a -> Response a -> Brain p t a
adjustActionCounts b r = set actionCounts cs' b
  where a = _action r
        cs = _actionCounts b
        cs' = M.alter inc a cs
        inc Nothing = Just 1
        inc (Just n) = Just (n+1)

-- | Evaluates the input patterns and the current condition.
--   Returns the "signature" (differences between the input pattern
--   and each model in the classifier) of each input pattern,
--   and the updated brain.
classifyInputs
  :: Brain p t a -> [p]
    -> ([Cl.Label], [[(Cl.Label, GSOM.Difference)]], Brain p t a)
classifyInputs b ps = (bmus, ds, b')
  where (bmus, ds, c') = Cl.classifySetAndTrain (_classifier b) ps
        b' = set classifier c' b

generateScenarios
  :: Condition -> [[(Cl.Label, GSOM.Difference)]]
    -> [(Scenario, Probability)]
generateScenarios c lds = map f . hypothesise $ lds
  where f (ls, p) = (Scenario ls c, p)

sumByAction :: Eq a => [Response a] -> [(a, PM1Double)]
sumByAction rs = map sumByAction' rss
  where rss = groupBy sameAction rs
        sameAction x y = _action x == _action y

sumByAction' :: [Response a] -> (a, PM1Double)
sumByAction' rs = (a, o)
  where a = _action $ head rs
        o = sum $ map _outcome rs

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
predictAll
  :: Eq a
    =>  Brain p t a -> [(Response a, Probability)]
      -> ([(Response a, Probability, P.Label, PM1Double)],
          Brain p t a)
predictAll b rps = foldl' predictOne ([], b) rps

predictOne
  :: Eq a
    => ([(Response a, Probability, P.Label, PM1Double)],  Brain p t a)
      -> (Response a, Probability)
        -> ([(Response a, Probability, P.Label, PM1Double)],
            Brain p t a)
predictOne (rplos, b) (r, p) = ((r', p, l, o):rplos, b {_predictor = d})
  where (r', l, o, d) = P.predict (_predictor b) r p

-- | Considers whether the wain is happier or not as a result of the
--   last action it took, and modifies the decision models accordingly.
--   Returns the updated brain, and the error in the brain's
--   prediction of the outcome.
reflect
  :: Eq a
    => Brain p t a -> Response a -> Condition
      -> (Brain p t a, Double)
reflect b r cAfter = (set predictor d' b, err)
  where deltaH = uiToDouble (happiness b cAfter)
                   - uiToDouble (happiness b cBefore)
        cBefore = _condition . _scenario $ r
        predictedDeltaH = pm1ToDouble . _outcome $ r
        d' = GSOM.train (_predictor b)
               (r {_outcome = doubleToPM1 deltaH})
        err = abs (deltaH - predictedDeltaH)

-- | Teaches the brain that the last action taken was a perfect one
--   (increased happiness by 1).
--   This can be used to help children learn by observing their parents.
--   It can also be used to allow wains to learn from others.
imprint
  :: Eq a
    => Brain p t a -> [p] -> a -> Condition -> Brain p t a
imprint b ps a c = set predictor d' b'
  where (_, lds, b') = classifyInputs b ps
        s = fst . maximumBy (comparing snd) . generateScenarios c $ lds
        d = _predictor b'
        d' = P.imprint d s a

-- | Returns @True@ if both the classifier and predictor are valid
--   according to @somOK@; returns @False@ otherwise.
brainOK :: Eq a => Brain p t a -> Bool
brainOK b = GSOM.somOK (_classifier b) && GSOM.somOK (_predictor b)
              && muserOK (_muser b)

happiness :: Brain p t a -> Condition -> UIDouble
happiness b = weightedSum (_happinessWeights b)

decisionQuality :: Brain p t a -> Int
decisionQuality = GSOM.discrimination . M.elems . _actionCounts

scenarioReport :: [(Scenario, Probability)] -> [String]
scenarioReport = map f
  where f (s, p) = pretty s ++ " prob: " ++ prettyProbability p

responseReport
  :: Show a
    => [(Response a, Probability, P.Label, PM1Double)] -> [String]
responseReport = map f
  where f (r, p, l, o) = pretty r ++ " prob: " ++ prettyProbability p
          ++ " based on model " ++ show l ++ " raw outcome: "
          ++ printf "%.3f" (pm1ToDouble o)

decisionReport :: Show a => [(a, PM1Double)] -> [String]
decisionReport = map f
  where f (a, o) = "predicted outcome of " ++ show a ++ " is "
          ++ printf "%.3f" (pm1ToDouble o)

prettyProbability :: Probability -> String
prettyProbability p = show p' ++ "%"
  where p' = round (uiToDouble p * 100) :: Int
