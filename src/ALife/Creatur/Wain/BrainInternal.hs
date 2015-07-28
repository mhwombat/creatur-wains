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

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import qualified ALife.Creatur.Wain.Classifier as Cl
import qualified ALife.Creatur.Wain.Predictor as P
import ALife.Creatur.Wain.Muser (Muser, generateResponses,
  mostLikelyScenarios, depth)
import qualified ALife.Creatur.Wain.GeneticSOM as GSOM
import ALife.Creatur.Wain.Response (Response(..))
import ALife.Creatur.Wain.Scenario (Scenario(..))
import ALife.Creatur.Wain.Statistics (Statistical, stats, prefix,
  iStat, dStat)
import ALife.Creatur.Wain.PlusMinusOne (doubleToPM1, pm1ToDouble)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble)
import ALife.Creatur.Wain.Weights (Weights, weightAt, weightedSum)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.List (maximumBy, foldl')
import Data.Ord (comparing)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Brain p t a = Brain
  {
    -- | Component that categorises and identifies patterns
    _classifier :: Cl.Classifier p t,
    -- | Component that generates response models for consideration
    _muser :: Muser,
    -- | Component that decides what actions to take
    _predictor :: P.Predictor a,
    -- | Weights for evaluating happiness
    _happinessWeights :: Weights
  } deriving (Generic, Eq, NFData)
makeLenses ''Brain

instance (Serialize p, Serialize t, Serialize a, Eq a, GSOM.Tweaker t,
  p ~ GSOM.Pattern t)
    => Serialize (Brain p t a)

instance (Diploid p, Diploid t, Diploid a, Eq a, GSOM.Tweaker t,
  p ~ GSOM.Pattern t)
    => Diploid (Brain p t a)

instance (Eq a, Ord a)
      => Statistical (Brain p t a) where
  stats (Brain c pos d hw) = map (prefix "classifier ") (stats c)
    ++ (stats pos)
    ++ (stats d)
    ++ map (prefix "predictor ") (stats d)
    ++ [ iStat "DQ" $ P.predictorQuality d,
         dStat "energyWeight" . uiToDouble $ hw `weightAt` 0,
         dStat "passionWeight" . uiToDouble $ hw `weightAt` 1,
         dStat "litterSizeWeight" . uiToDouble $ hw `weightAt` 2]

instance (Show p, Show a, Show t, Eq a)
      => Show (Brain p t a) where
  show (Brain c pos d hw) = "Brain (" ++ show c ++ ") ("
    ++ show pos ++ ") (" ++ show d ++ ") (" ++ show hw ++ ")"

-- | Returns @True@ if both the classifier and predictor are valid
--   according to @somOK@; returns @False@ otherwise.
brainOK :: Eq a => Brain p t a -> Bool
brainOK b = GSOM.somOK (_classifier b) && GSOM.somOK (_predictor b)
              && view (muser . depth) b > 0

instance (Genetic p, Genetic t, Genetic a, Eq a, GSOM.Tweaker t,
          p ~ GSOM.Pattern t)
    => Genetic (Brain p t a)

-- | Chooses a response based on the stimuli (input patterns) and
--   the wain's condition.
--   Returns the chosen response, the updated brain, the responses it
--   considered (with outcome predictions), and the novelty of each
--   input pattern.
chooseAction
  :: (Eq a, Enum a, Bounded a)
      => Brain p t a -> [p] -> [UIDouble]
        -> ([[(Cl.Label, UIDouble)]], [(Response a, P.Label)],
            Response a, Brain p t a)
chooseAction b ps c = (lds, rls, r, b'')
  where (lds, b') = assessSituation b ps
        rps = generateResponses (_muser b) lds c
        (rls, b'') = predictAll b' rps
        (r, _) = maximumBy bestOutcome rls
        bestOutcome = comparing (_outcome . fst)

-- | Evaluates the input patterns and the current condition.
--   Returns the "signature" (differences between the input pattern
--   and each model in the classifier) of each input pattern,
--   and the updated brain.
assessSituation
  :: Brain p t a -> [p] -> ([[(Cl.Label, UIDouble)]], Brain p t a)
assessSituation b ps = (ds, b')
  where (ds, c') = Cl.classifySetAndTrain (_classifier b) ps
        b' = set classifier c' b

-- | Predicts the outcomes of a set of responses based on the Predictor
--   models, and updates the outcome field in each response.
predictAll
  :: Eq a
    =>  Brain p t a -> [(Response a, UIDouble)]
      -> ([(Response a, P.Label)],  Brain p t a)
predictAll b rcs = foldl' predictOne ([], b) rcs

predictOne
  :: Eq a
    => ([(Response a, P.Label)],  Brain p t a)
      -> (Response a, UIDouble)
        -> ([(Response a, P.Label)],  Brain p t a)
predictOne (rls, b) (r, k) = ((r', l):rls, b {_predictor = d})
  where (r', l, d) = P.predict (_predictor b) (r, k)

-- | Considers whether the wain is happier or not as a result of the
--   last action it took, and modifies the decision models accordingly.
--   Returns the updated brain, and the error in the brain's
--   prediction of the outcome.
reflect
  :: Eq a
    => Brain p t a -> Response a -> [UIDouble]
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
    => Brain p t a -> [p] -> a -> [UIDouble] -> Brain p t a
imprint b ps a c = set predictor d' b'
  where (lds, b') = assessSituation b ps
        (s, _) = head $ mostLikelyScenarios (_muser b) c lds
        d = _predictor b
        d' = P.imprint d s a

happiness :: Brain p t a -> [UIDouble] -> UIDouble
happiness b = weightedSum (_happinessWeights b)
