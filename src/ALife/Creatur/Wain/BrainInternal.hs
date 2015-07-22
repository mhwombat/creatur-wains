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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.BrainInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import qualified ALife.Creatur.Wain.Classifier as Cl
import qualified ALife.Creatur.Wain.Decider as D
import qualified ALife.Creatur.Wain.GeneticSOM as GSOM
import ALife.Creatur.Wain.Response (Response(..), responseSet)
import ALife.Creatur.Wain.Scenario (Scenario(..))
import ALife.Creatur.Wain.Statistics (Statistical, stats, prefix,
  iStat, dStat)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, doubleToPM1,
  pm1ToDouble)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble)
import ALife.Creatur.Wain.Weights (Weights, weightAt, weightedSum)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Brain p t a = Brain
  {
    -- | Component that categorises and identifies patterns
    _classifier :: Cl.Classifier p t,
    -- | Component that decides what actions to take
    _decider :: D.Decider a,
    -- | Weights for evaluating happiness
    _happinessWeights :: Weights
  } deriving (Generic, Eq, NFData)
makeLenses ''Brain

-- deriving instance (Eq p, Eq a, Eq t)
--     => Eq (Brain p t a)

instance (Serialize p, Serialize t, Serialize a, Eq a, GSOM.Tweaker t, 
  p ~ GSOM.Pattern t)
    => Serialize (Brain p t a)
         
instance (Diploid p, Diploid t, Diploid a, Eq a, GSOM.Tweaker t,
  p ~ GSOM.Pattern t)
    => Diploid (Brain p t a)

instance (Eq a, Ord a)
      => Statistical (Brain p t a) where
  stats (Brain c d hw) = map (prefix "classifier ") (stats c)
    ++ map (prefix "decider ") (stats d)
    ++ [ iStat "DQ" $ D.deciderQuality d,
         dStat "energyWeight" . uiToDouble $ hw `weightAt` 0,
         dStat "passionWeight" . uiToDouble $ hw `weightAt` 1,
         dStat "litterSizeWeight" . uiToDouble $ hw `weightAt` 2]

instance (Show p, Show a, Show t, Eq a)
      => Show (Brain p t a) where
  show (Brain c d hw) = "Brain (" ++ show c ++ ") (" ++ show d
     ++ ") (" ++ show hw ++ ")"

-- | Returns @True@ if both the classifier and decider are valid
--   according to @somOK@; returns @False@ otherwise.
brainOK :: Eq a => Brain p t a -> Bool
brainOK b = GSOM.somOK (_classifier b) && GSOM.somOK (_decider b)

instance (Genetic p, Genetic t, Genetic a, Eq a, GSOM.Tweaker t,
          p ~ GSOM.Pattern t)
    => Genetic (Brain p t a)

-- | Chooses a response based on the stimuli (input patterns).
--   Returns the chosen response, the updated brain, the responses it
--   considered (with outcome predictions), and the novelty of each
--   input pattern.
chooseAction
  :: (Eq a, Enum a, Bounded a)
      => Brain p t a -> [p] -> [UIDouble] -> PM1Double
        -> ([Cl.Label], D.Label, Response a, Brain p t a,
              [(Response a, D.Label)], [UIDouble])
chooseAction b ps c o = (ks, k, r, b'', rks, ns)
  where (ks, s, ns, b') = assessSituation b ps c
        (rks, b'') = predict b' $ responseSet s o
        (r, k) = maximumBy bestOutcome rks
        bestOutcome = comparing (_outcome . fst)

-- | Evaluates the input patterns and the current condition.
--   Returns the scenario, the novelty of each input pattern, and
--   the updated brain.
--   See @Scenario@ for more information.
assessSituation
  :: Brain p t a -> [p] ->  [UIDouble]
    -> ([Cl.Label], Scenario, [UIDouble], Brain p t a)
assessSituation b ps c = (ks, s, ns, b')
  where (ks, ns, ds, c') = Cl.classifySetAndTrain (_classifier b) ps
        b' = set classifier c' b
        s = Scenario ds c

-- | Predicts the outcome of possible responses based on the brain's
--   decider models, and updates the outcome field in each response.
predict
  :: Eq a
    => Brain p t a -> [Response a] -> ([(Response a, D.Label)], Brain p t a)
predict b rs = (rls, b {_decider = d})
  where (rls, d) = D.predictAll (_decider b) rs

-- | Considers whether the wain is happier or not as a result of the
--   last action it took, and modifies the decision models accordingly.
--   Returns the updated brain, and the error in the brain's
--   prediction of the outcome.
reflect
  :: Eq a
    => Brain p t a -> Response a -> [UIDouble]
      -> (Brain p t a, Double)
reflect b r cAfter = (set decider d' b, err)
  where deltaH = uiToDouble (happiness b cAfter)
                   - uiToDouble (happiness b cBefore)
        cBefore = _condition . _scenario $ r
        predictedDeltaH = pm1ToDouble . _outcome $ r
        d' = GSOM.train (_decider b)
               (r {_outcome = doubleToPM1 deltaH})
        err = abs (deltaH - predictedDeltaH)

-- | Teaches the brain that the last action taken was a perfect one
--   (increased happiness by 1).
--   This can be used to help children learn by observing their parents.
--   It can also be used to allow wains to learn from others.
imprint
  :: Eq a
    => Brain p t a -> [p] -> a -> [UIDouble] -> Brain p t a
imprint b ps a c = set decider d' b'
  where (_, s, _, b') = assessSituation b ps c
        d = _decider b
        d' = D.imprint d s a

happiness :: Brain p t a -> [UIDouble] -> UIDouble
happiness b = weightedSum (_happinessWeights b)
