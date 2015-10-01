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
import ALife.Creatur.Wain.Muser (Muser, generateResponses, muserOK,
  _defaultOutcomes)
import qualified ALife.Creatur.Wain.GeneticSOM as GSOM
import ALife.Creatur.Wain.Response (Response(..))
import ALife.Creatur.Wain.Probability (Probability, hypothesise)
import ALife.Creatur.Wain.Statistics (Statistical, stats, prefix,
  iStat, dStat)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, doubleToPM1,
  pm1ToDouble)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble,
  forceDoubleToUI)
import ALife.Creatur.Wain.Weights (Weights, weightAt, weightedSum,
  numWeights)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Function (on)
import Data.List (intercalate, maximumBy, groupBy, sortBy, foldl')
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Printf (printf)

-- | A wain's condition
type Condition = [UIDouble]

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
    -- | Used to break ties when actions seem equally promising
    _tiebreaker :: Word8,
    -- | When a wain is taught a response, or learns it by observing
    --   another wain (rather than through personal experience),
    --   it will assume the action has the following outcomes.
    _imprintOutcomes :: [PM1Double],
    -- | Number of times each action has been used
    _actionCounts :: M.Map a Int
  } deriving (Generic, Eq, NFData)
makeLenses ''Brain

makeBrain
  :: Cl.Classifier p t -> Muser -> P.Predictor a -> Weights -> Word8
    -> [PM1Double] -> Brain p t a
makeBrain c m p hw t ios
  | numWeights hw /= length (_defaultOutcomes m)
      = error "weights and default outcomes are different lengths"
  | numWeights hw /= length ios
      = error "weights and imprint outcomes are different lengths"
  | otherwise
      = Brain c m p hw t ios M.empty

instance (Serialize p, Serialize t, Serialize a, Eq a, Ord a,
  GSOM.Tweaker t, p ~ GSOM.Pattern t)
    => Serialize (Brain p t a)

instance (Diploid p, Diploid t, Diploid a, Eq a, Ord a, GSOM.Tweaker t,
  p ~ GSOM.Pattern t)
    => Diploid (Brain p t a)

instance (Eq a, Ord a)
      => Statistical (Brain p t a) where
  stats b@(Brain c m p hw t ios _) = map (prefix "classifier ") (stats c)
    ++ (stats m)
    ++ map (prefix "predictor ") (stats p)
    ++ [ iStat "DQ" $ decisionQuality b,
         dStat "energyWeight" . uiToDouble $ hw `weightAt` 0,
         dStat "passionWeight" . uiToDouble $ hw `weightAt` 1,
         dStat "boredomWeight" . uiToDouble $ hw `weightAt` 2,
         dStat "litterSizeWeight" . uiToDouble $ hw `weightAt` 3,
         dStat "energyImprint" . pm1ToDouble $ ios !! 0,
         dStat "passionImprint" . pm1ToDouble $ ios !! 1,
         dStat "boredomImprint" . pm1ToDouble $ ios !! 2,
         dStat "litterSizeImprint" . pm1ToDouble $ ios !! 3,
         iStat "tiebreaker" t ]

instance (Show p, Show a, Show t, Eq a)
      => Show (Brain p t a) where
  show (Brain c m p hw t ios ks) = "Brain (" ++ show c ++ ") ("
    ++ show m ++ ") (" ++ show p ++ ") (" ++ show hw ++ ") "
    ++ show t ++ " " ++ show ios ++ " (" ++ show ks ++ ")"

instance (Genetic p, Genetic t, Genetic a, Eq a, Ord a, GSOM.Tweaker t,
          p ~ GSOM.Pattern t)
    => Genetic (Brain p t a) where
    put (Brain c m p hw t ios _)
      = put c >> put m >> put p >> put hw >> put t >> put ios
    get = do
      c <- get
      m <- get
      p <- get
      hw <- get
      t <- get
      ios <- get
      return $
        Brain <$> c <*> m <*> p  <*> hw <*> t <*> ios  <*> pure M.empty

-- | Chooses a response based on the stimuli (input patterns) and
--   the wain's condition.
--   Returns:
--
--   * for each object in the stimuli, the difference between that
--     object and each classifier model
--   * the hypotheses considered, together with the probability that
--     each hypothesis is true
--   * the responses considered (with predicted outcomes filled in)
--     together with the probability that each response is based on a
--     correct hypothesis, the label of the predictor model upon which
--     the response is based, and the expected happiness after the
--     response
--   * the actions considered, paired with the expected happiness after
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
      => Brain p t a -> [p] -> Condition
        -> ([[(Cl.Label, GSOM.Difference)]],
            [([Cl.Label], Probability)],
            [(Response a, Probability, P.Label, [PM1Double])],
            [(a, [PM1Double], UIDouble)],
            Response a,
            Brain p t a)
chooseAction b ps c = (lds, sps, rplos, aohs, r, b3)
  where (cBmus, lds, b2) = classifyInputs b ps
        sps = hypothesise lds
        sps' = filter (and . map (GSOM.hasModel (_classifier b)) . fst) sps
        spsSafe = if null sps'
                    then sps -- nothing to base estimate on; have to guess
                    else sps'
        rps = generateResponses (_muser b2) spsSafe
        rplos = predictAll b2 rps
        rs = map (\(r1, _, _, _) -> r1) rplos
        aos = sumByAction $ rs
        aohs = map (fillInAdjustedHappiness b2 c) aos
        (a, os, _) = chooseAny b . maximaBy thirdOfTriple $ aohs
        b3 = adjustActionCounts b2 r
        r = Response cBmus a os

onlyModelsIn :: Brain p t a -> [(Cl.Label, GSOM.Difference)] -> Bool
onlyModelsIn b = and . map (GSOM.hasModel (_classifier b) . fst)

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f = map snd . head . reverse . groupBy ((==) `on` fst)
               . sortBy (comparing fst) . map (\x -> (f x, x))

chooseAny :: Brain p t a -> [x] -> x
chooseAny b xs = xs !! (seed `mod` n)
  where seed = fromIntegral $ _tiebreaker b
        n = length xs -- the list will be short

thirdOfTriple :: (a, b, c) -> c
thirdOfTriple (_, _, x) = x

fillInAdjustedHappiness
  :: Brain p t a -> Condition -> (a, [PM1Double])
    -> (a, [PM1Double], UIDouble)
fillInAdjustedHappiness b c (a, os) = (a, os, adjustedHappiness b c os)

adjustedHappiness :: Brain p t a -> Condition -> [PM1Double] -> UIDouble
adjustedHappiness b c = happiness b . adjustCondition c

adjustCondition :: Condition -> [PM1Double] -> Condition
adjustCondition c os =
  map forceDoubleToUI $
    zipWith (+) (map uiToDouble c) (map pm1ToDouble os)

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
    -> ([Cl.Label], [[(Cl.Label, GSOM.Difference)]],
        Brain p t a)
classifyInputs b ps = (bmus, ds, b')
  where (bmus, ds, c') = Cl.classifySetAndTrain (_classifier b) ps
        b' = set classifier c' b

sumByAction :: Eq a => [Response a] -> [(a, [PM1Double])]
sumByAction rs = map sumByAction' rss
  where rss = groupBy sameAction rs
        sameAction x y = _action x == _action y

sumByAction' :: [Response a] -> (a, [PM1Double])
sumByAction' rs = (a, os)
  where a = _action $ head rs
        os = sumTermByTerm $ map _outcomes rs

sumTermByTerm :: Num a => [[a]] -> [a]
sumTermByTerm [] = []
sumTermByTerm (xs:[]) = xs
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
predictAll
  :: Eq a
    => Brain p t a -> [(Response a, Probability)]
      -> [(Response a, Probability, P.Label, [PM1Double])]
predictAll b rps = foldl' (predictOne b) [] rps

predictOne
  :: Eq a
    => Brain p t a
      -> [(Response a, Probability, P.Label,  [PM1Double])]
        -> (Response a, Probability)
          -> ([(Response a, Probability, P.Label, [PM1Double])])
predictOne b rplos (r, p) = (r', p, l, os):rplos
  where (r', l, os, _) = P.predict (_predictor b) r p

-- | Considers whether the wain is happier or not as a result of the
--   last action it took, and modifies the decision models accordingly.
--   Returns the updated brain, and the error in the brain's
--   prediction of the outcome.
reflect
  :: Eq a
    => Brain p t a -> Response a -> Condition -> Condition
      -> (Brain p t a, Double)
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

-- | Teaches the brain that the last action taken was a perfect one
--   (increased happiness by 1).
--   This can be used to help children learn by observing their parents.
--   It can also be used to allow wains to learn from others.
imprint :: Eq a => Brain p t a -> [p] -> a -> Brain p t a
imprint b ps a = set predictor d' b'
  where (_, lds, b') = classifyInputs b ps
        d = _predictor b'
        ls = fst . maximumBy (comparing snd) . hypothesise $ lds
        r = Response ls a (_imprintOutcomes b)
        d' = GSOM.train d r

-- | Returns @True@ if both the classifier and predictor are valid
--   according to @somOK@; returns @False@ otherwise.
brainOK :: Eq a => Brain p t a -> Bool
brainOK b = GSOM.somOK (_classifier b) && GSOM.somOK (_predictor b)
              && muserOK (_muser b)

happiness :: Brain p t a -> Condition -> UIDouble
happiness b = weightedSum (_happinessWeights b)

decisionQuality :: Brain p t a -> Int
decisionQuality = GSOM.discrimination . M.elems . _actionCounts

scenarioReport :: [([Cl.Label], Probability)] -> [String]
scenarioReport = map f
  where f (s, p) = show s ++ " prob: " ++ prettyProbability p

responseReport
  :: Show a
    => [(Response a, Probability, P.Label, [PM1Double])] -> [String]
responseReport = map f
  where f (r, p, l, os) = pretty r ++ " prob: " ++ prettyProbability p
          ++ " based on model " ++ show l ++ " raw outcomes: "
          ++ intercalate " " (map (printf "%.3f" . pm1ToDouble) os)

decisionReport :: Show a => [(a, [PM1Double], UIDouble)] -> [String]
decisionReport = map f
  where f (a, os, h) = "predicted outcomes of " ++ show a ++ " are "
          ++ intercalate " " (map (printf "%.3f" . pm1ToDouble) os)
          ++ " with resulting happiness "
          ++ printf "%.3f" (uiToDouble h)

prettyProbability :: Probability -> String
prettyProbability p = printf "%.3f" (uiToDouble p * 100) ++ "%"

