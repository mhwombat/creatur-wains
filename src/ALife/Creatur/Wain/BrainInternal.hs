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
import qualified ALife.Creatur.Wain.Classifier as C
import qualified ALife.Creatur.Wain.Decider as D
import qualified ALife.Creatur.Wain.GeneticSOM as GSOM
import ALife.Creatur.Wain.Condition (Condition, happiness)
import ALife.Creatur.Wain.Response (Response(..), setOutcome)
import ALife.Creatur.Wain.Scenario (Scenario(..))
import ALife.Creatur.Wain.Statistics (Statistical, stats, prefix)
import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Data.Datamining.Pattern (Pattern, Metric)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe, fromJust)
import Data.Ord (comparing)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Brain p a = Brain
  {
    -- | Component that categorises and identifies patterns
    _classifier :: C.Classifier p,
    -- | Component that decides what actions to take
    _decider :: D.Decider a
  } deriving (Generic)

makeLenses ''Brain

deriving instance (Eq p, Eq a, Pattern p, Metric p ~ Double) 
    => Eq (Brain p a)

instance (Serialize p, Serialize a, Pattern p, Eq a, Metric p ~ Double)
    => Serialize (Brain p a)
         
instance (Diploid p, Diploid a, Pattern p, Eq a, Metric p ~ Double)
    => Diploid (Brain p a)

instance (Pattern p, Metric p ~ Double, Eq a)
      => Statistical (Brain p a) where
  stats (Brain c d) = map (prefix "classifier ") (stats c)
    ++ map (prefix "decider ") (stats d) 

instance (Pattern p, Show p, Show (Metric p), Ord (Metric p), Show a, Eq a)
      => Show (Brain p a) where
  show (Brain c d) = "Brain (" ++ show c ++ ") (" ++ show d ++ ")"

-- | Returns @True@ if both the classifier and decider are valid
--   according to @somOK@; returns @False@ otherwise.
brainOK
  :: (Pattern p, Ord (Metric p), Metric p ~ Double, Eq a)
    => Brain p a -> Bool
brainOK b = C.classifierOK (_classifier b)
              && D.deciderOK (_decider b)

instance (Genetic p, Genetic a, Pattern p, Metric p ~ Double, Eq a)
    => Genetic (Brain p a) where
  put (Brain c d) = G.put c >> G.put d
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
    return $ Brain <$> c <*> d

-- | Presents stimuli to a brain, and returns the the action it chooses
--   to take, the scenario the brain considered, and the responses it
--   considered (with outcome predictions).
chooseAction
  :: (Pattern p, Metric p ~ Double, Eq a, Enum a, Bounded a)
      => Brain p a -> p -> p -> Condition
        -> (Response a, Brain p a, [(Response a, D.Label)])
chooseAction b p1 p2 c
  = (r, b', consideredResponses)
  where (s, b') = assessSituation b p1 p2 c
        consideredResponses = map (predict b s) $ knownActions b
        r = maximumBy f $ map fst consideredResponses
        f = comparing (fromMaybe 0 . _outcome)

-- | Returns a scenario, based on the two input patterns
--   (direct object and indirect object) and the current condition.
--   See @Scenario@ for more information.
assessSituation
  :: (Pattern p, Metric p ~ Double)
    => Brain p a -> p -> p -> Condition -> (Scenario, Brain p a)
assessSituation b p1 p2 c = (s, b2)
  where (sig1, b1) = classify p1 b
        (sig2, b2) = classify p2 b1
        s = Scenario sig1 sig2 c

-- | Updates the brain's classifier models based on the stimulus
--   (input).
--   Returns the index (grid location) of the model that most closely
--   matches the input pattern, the novelty of the input pattern,
--   the adjusted novelty, and the updated brain (which has learned
--   from the classification experience).
classify
  :: (Pattern p, Metric p ~ Double)
    => p -> Brain p a -> ([Metric p], Brain p a)
classify s b = (sig, b')
  where (sig, c') = C.classify (_classifier b) s
        b' = set classifier c' b

-- | Returns the list of actions that this brain "knows".
knownActions :: (Eq a) => Brain p a -> [a]
knownActions = D.knownActions . _decider

-- | Predicts the outcome of a response based on the brain's decider
--   models, and updates the outcome field in that response.
predict :: (Eq a) => Brain p a -> Scenario -> a -> (Response a, D.Label)
predict b = D.predict (_decider b)

-- | Considers whether the wain is happier or not as a result of the
--   last action it took, and modifies the decision models accordingly.
--   Returns the updated brain, and the error in the brain's
--   prediction of the outcome.
reflect
  :: (Pattern p, Metric p ~ Double, Eq a)
    => Brain p a -> Double -> Response a -> Condition -> (Brain p a, Double)
reflect b x r cAfter = (set decider d' b, err)
  where deltaH = happiness x cAfter - happiness x cBefore
        cBefore = _condition . _scenario $ r
        predictedDeltaH = fromJust . _outcome $ r
        (_, _, d') = GSOM.reportAndTrain (_decider b) (r `setOutcome` deltaH)
        err = abs (deltaH - predictedDeltaH)

-- | Teaches the brain that the last action taken was a perfect one.
--   This can be used to help children learn by observing their parents.
--   It can also be used to allow wains to learn from others.
imprint
  :: (Pattern p, Metric p ~ Double, Eq a)
    => Brain p a -> p -> p -> a -> Condition -> Brain p a
imprint b p1 p2 a c = set decider d' b'
  where (s, b') = assessSituation b p1 p2 c
        d = _decider b
        d' = D.imprint d s a

