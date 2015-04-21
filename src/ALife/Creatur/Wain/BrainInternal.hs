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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.BrainInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Genetics.Diploid (Diploid)
import qualified ALife.Creatur.Wain.Classifier as Cl
import qualified ALife.Creatur.Wain.Decider as D
import qualified ALife.Creatur.Wain.GeneticSOM as GSOM
import qualified ALife.Creatur.Wain.Condition as Cd
import ALife.Creatur.Wain.Response (Response(..), setOutcome)
import ALife.Creatur.Wain.Scenario (Scenario(..))
import ALife.Creatur.Wain.Statistics (Statistical, stats, prefix,
  iStat, uiStat)
import ALife.Creatur.Wain.Weights (Weights, weightAt)
import Control.Lens
import Data.List (maximumBy)
import Data.Maybe (fromMaybe, fromJust)
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
  } deriving (Generic)

makeLenses ''Brain

deriving instance (Eq p, Eq a, Eq t)
    => Eq (Brain p t a)

instance (Serialize p, Serialize t, Serialize a, Eq a, GSOM.Thinker t, 
  p ~ GSOM.Pattern t)
    => Serialize (Brain p t a)
         
instance (Diploid p, Diploid t, Diploid a, Eq a, GSOM.Thinker t,
  p ~ GSOM.Pattern t)
    => Diploid (Brain p t a)

instance (Eq a, Ord a)
      => Statistical (Brain p t a) where
  stats (Brain c d hw) = map (prefix "classifier ") (stats c)
    ++ map (prefix "decider ") (stats d)
    ++ [ iStat "DQ" $ D.deciderQuality d,
         uiStat "energyWeight" $ hw `weightAt` 0,
         uiStat "passionWeight" $ hw `weightAt` 1,
         uiStat "litterSizeWeight" $ hw `weightAt` 2]

instance (Show p, Show a, Show t, Eq a)
      => Show (Brain p t a) where
  show (Brain c d hw) = "Brain (" ++ show c ++ ") (" ++ show d
     ++ ") (" ++ show hw ++ ")"

-- | Returns @True@ if both the classifier and decider are valid
--   according to @somOK@; returns @False@ otherwise.
brainOK :: Eq a => Brain p t a -> Bool
brainOK b = Cl.classifierOK (_classifier b)
              && D.deciderOK (_decider b)

instance (Genetic p, Genetic t, Genetic a, Eq a, GSOM.Thinker t, 
  p ~ GSOM.Pattern t)
    => Genetic (Brain p t a) where
  put (Brain c d hw) = G.put c >> G.put d >> G.put hw
  get = do
    c0 <- G.get
    let c = case c0 of
          (Left xs) -> Left ("Classifier:":xs)
          (Right c1) ->
            if GSOM.numModels c1 == 0
               then Left ["Classifier has no models"]
               else Right c1
    d0 <- G.get
    let d = case d0 of
          (Left xs) -> Left ("Decider:":xs)
          (Right d1) ->
            if GSOM.numModels d1 == 0
               then Left ["Decider has no models"]
               else Right d1
    hw <- G.get
    return $ Brain <$> c <*> d <*> hw

-- | Presents stimuli to a brain, and returns the the action it chooses
--   to take, the scenario the brain considered, and the responses it
--   considered (with outcome predictions).
chooseAction
  :: (Eq a, Enum a, Bounded a)
      => Brain p t a -> [p] -> Cd.Condition
        -> (Response a, Brain p t a, [(Response a, D.Label)], [Double])
chooseAction b ps c
  = (r, b', consideredResponses, ns)
  where (s, ns, b') = assessSituation b ps c
        consideredResponses = map (predict b s) $ knownActions b
        r = maximumBy f $ map fst consideredResponses
        f = comparing (fromMaybe 0 . _outcome)

-- | Returns a scenario, based on the input patterns and the current
--   condition.
--   See @Scenario@ for more information.
assessSituation
  :: Brain p t a -> [p] -> Cd.Condition -> (Scenario, [Double], Brain p t a)
assessSituation b ps c = (s, ns, b')
  where (ds, ns, c') = Cl.classifyAll (_classifier b) ps
        b' = set classifier c' b
        s = Scenario ds c

-- -- | Updates the brain's classifier models based on the stimulus
-- --   (input).
-- --   Returns the index (grid location) of the model that most closely
-- --   matches the input pattern, the novelty of the input pattern,
-- --   the adjusted novelty, and the updated brain (which has learned
-- --   from the classification experience).
-- classify :: p -> Brain p t a -> ([Double], Brain p t a)
-- classify s b = (sig, b')
--   where (sig, c') = Cl.classify (_classifier b) s
--         b' = set classifier c' b

-- | Returns the list of actions that this brain "knows".
knownActions :: Eq a => Brain p t a -> [a]
knownActions = D.knownActions . _decider

-- | Predicts the outcome of a response based on the brain's decider
--   models, and updates the outcome field in that response.
predict :: Eq a => Brain p t a -> Scenario -> a -> (Response a, D.Label)
predict b = D.predict (_decider b)

-- | Considers whether the wain is happier or not as a result of the
--   last action it took, and modifies the decision models accordingly.
--   Returns the updated brain, and the error in the brain's
--   prediction of the outcome.
reflect
  :: Eq a
    => Brain p t a -> Response a -> Cd.Condition -> (Brain p t a, Double)
reflect b r cAfter = (set decider d' b, err)
  where deltaH = happiness b cAfter - happiness b cBefore
        cBefore = _condition . _scenario $ r
        predictedDeltaH = fromJust . _outcome $ r
        (_, _, _, d') = GSOM.reportAndTrain (_decider b) (r `setOutcome` deltaH)
        err = abs (deltaH - predictedDeltaH)

-- | Teaches the brain that the last action taken was a perfect one.
--   This can be used to help children learn by observing their parents.
--   It can also be used to allow wains to learn from others.
imprint
  :: Eq a
    => Brain p t a -> [p] -> a -> Cd.Condition -> Brain p t a
imprint b ps a c = set decider d' b'
  where (s, _, b') = assessSituation b ps c
        d = _decider b
        d' = D.imprint d s a

happiness :: Brain p t a -> Cd.Condition -> Double
happiness b = Cd.happiness (_happinessWeights b)
