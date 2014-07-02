------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.BrainInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal implementation details
-- A module containing private Brain internals.
-- Most developers should use Brain instead.
-- This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleContexts,
    StandaloneDeriving #-}
module ALife.Creatur.Wain.BrainInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Genetics.Diploid (Diploid)
import qualified ALife.Creatur.Wain.Classifier as C
import qualified ALife.Creatur.Wain.Decider as D
import ALife.Creatur.Wain.Condition (Condition(..), happiness)
import ALife.Creatur.Wain.Response (Response(..), setOutcome)
import ALife.Creatur.Wain.Scenario (Scenario(..))
import ALife.Creatur.Wain.Statistics (Statistical, stats, prefix)
import Control.Applicative ((<$>), (<*>))
import Data.Datamining.Pattern (Pattern, Metric)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe, fromJust)
import Data.Ord (comparing)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Brain p a = Brain
  {
    -- | Component that categorises and identifies patterns
    classifier :: C.Classifier p,
    -- | Component that decides what actions to take
    decider :: D.Decider a
  } deriving (Generic)

deriving instance (Eq p, Eq a, Pattern p, Metric p ~ Double) 
    => Eq (Brain p a)

instance (Serialize p, Serialize a, Pattern p, Eq a, Metric p ~ Double)
    => Serialize (Brain p a)
         
instance (Diploid p, Diploid a, Pattern p, Eq a, Metric p ~ Double)
    => Diploid (Brain p a)

instance (Pattern p, Metric p ~ Double)
      => Statistical (Brain p a) where
  stats (Brain c d) = map (prefix "classifier ") (stats c)
    ++ map (prefix "decider ") (stats d)

instance (Pattern p, Show p, Show (Metric p), Ord (Metric p), Show a, Eq a)
      => Show (Brain p a) where
  show (Brain c d) = "Brain (" ++ show c ++ ") (" ++ show d ++ ")"

-- | Returns @True@ if both the classifier and decider have
--   at least one model each; returns @False@ otherwise.
brainOK
  :: (Pattern p, Ord (Metric p), Metric p ~ Double, Eq a)
    => Brain p a -> Bool
brainOK b = classifierOK && deciderOK
  where classifierOK = C.somOK $ classifier b
        deciderOK = D.somOK $ decider b

-- | Constructs a new brain.
buildBrain :: C.Classifier p -> D.Decider a -> Brain p a
buildBrain c d = Brain c d

instance (Genetic p, Genetic a, Pattern p, Metric p ~ Double, Eq a)
    => Genetic (Brain p a) where
  put (Brain c d) = G.put c >> G.put d
  get = do
    c0 <- G.get
    let c = case c0 of
          (Left xs) -> Left ("Classifier:":xs)
          (Right c1) ->
            if C.numModels c1 == 0
               then Left ["Classifier has no models"]
               else Right c1
    d0 <- G.get
    let d = case d0 of
          (Left xs) -> Left ("Decider:":xs)
          (Right d1) ->
            if D.numModels d1 == 0
               then Left ["Decider has no models"]
               else Right d1
    return $ buildBrain <$> c <*> d

-- | Presents two stimuli (direct object and indirect object)
--   to a brain, and returns the the action it chooses to take,
--   the scenario the brain considered, and
--   the responses it considered (with outcome predictions).
chooseAction
  :: (Pattern p, Metric p ~ Double, Eq a, Enum a,
    Bounded a, Show a)
      => p -> p -> Condition -> Brain p a
        -> (a, Scenario, [(Response a, D.Label)])
chooseAction p1 p2 c b = (action r, s, consideredResponses)
  where (_, _, s, _) = assessSituation p1 p2 c b
        consideredResponses = map (predict b) $ possibleResponses b s
        r = bestResponse . map fst $ consideredResponses
        -- Note: We don't need to return the updated brain. We'll do
        -- that later, when reflect is called.

bestResponse :: (Eq a, Enum a, Bounded a) => [Response a] -> Response a
bestResponse = maximumBy comp
  where comp = comparing (fromMaybe 0 . outcome)

-- | Returns a scenario, based on the two input patterns
--   (direct object and indirect object) and the current condition.
--   See @Scenario@ for more information.
assessSituation
  :: (Pattern p, Metric p ~ Double)
    => p -> p -> Condition -> Brain p a
      -> (C.Label, C.Label, Scenario, Brain p a)
assessSituation p1 p2 cond b = (l1, l2, sc, b2)
  where (l1, sig1, b1) = classify' p1 b
        (l2, sig2, b2) = classify' p2 b1
        sc = Scenario sig1 sig2 cond

-- | Updates the brain's classifier models based on the stimulus
--   (input).
--   Returns the index (grid location) of the model that most closely
--   matches the input pattern,
--   the differences between the input pattern and each model in the
--   classifier,
--   and the updated brain.
classify
  :: (Pattern p, Metric p ~ Double)
    => p -> Brain p a -> (C.Label, Brain p a)
classify p b = (label, b')
  where (label, _, b') = classify' p b

classify'
  :: (Pattern p, Metric p ~ Double)
    => p -> Brain p a -> (C.Label, [Metric p], Brain p a)
classify' s b = (label, sig, b')
  where (label, sig, c') = C.classify (classifier b) s
        b' = b { classifier = c' }

-- | Returns the list of actions that this brain "knows".
knownActions :: (Eq a) => Brain p a -> [a]
knownActions b = D.possibleActions $ decider b

-- | Returns the list of possible responses to the given scenario.
--   The outcome field in each response will be @Nothing@;
--   use @predict@ to calculate the outcome.
possibleResponses :: (Eq a) => Brain p a -> Scenario -> [Response a]
possibleResponses b s = D.possibleResponses (decider b) s

-- | Predicts the outcome of a response based on the brain's decider
--   models, and updates the outcome field in that response.
predict :: (Eq a) => Brain p a -> Response a -> (Response a, D.Label)
predict b r = D.predict (decider b) r

-- | Teaches a pattern + label to the brain.
learnLabel
  :: (Pattern p, Metric p ~ Double)
    => p -> C.Label -> Brain p a -> Brain p a
learnLabel p l b = b { classifier=C.learn p l (classifier b) }

-- | Considers whether the wain is happier or not as a result of the
--   last action it took, and modifies the decision models accordingly.
--   Returns the updated brain, and the error in the brain's
--   prediction of the outcome.
reflect
  :: (Pattern p, Metric p ~ Double, Eq a)
    => p -> p -> (Condition, Condition) -> a -> Brain p a
      -> (Brain p a, Double)
reflect p1 p2 (cBefore, cAfter) a b
  = reflect1 p1 p2 cBefore a deltaH b
  where deltaH = happiness cAfter - happiness cBefore

-- | Considers whether the wain is happier or not as a result of the
--   last action it took, and modifies the decision models accordingly.
reflect1
  :: (Pattern p, Metric p ~ Double, Eq a)
    => p -> p -> Condition -> a -> Double -> Brain p a -> (Brain p a, Double)
reflect1 p1 p2 cBefore a deltaH b = (b' {decider=d'}, err)
  where (_, _, s, b') = assessSituation p1 p2 cBefore b
        (r, _) = predict b $ Response s a Nothing
        predictedDeltaH = fromJust . outcome $ r
        r' = r `setOutcome` deltaH
        d' = D.feedback (decider b) r'
        err = abs (deltaH - predictedDeltaH)


-- | Teaches the brain that the last action taken was a perfect one.
--   This can be used to help children learn by observing their parents.
--   It can also be used to allow wains to learn from others.
imprint
  :: (Pattern p, Metric p ~ Double, Eq a)
    => p -> p -> a -> Brain p a -> Brain p a
imprint p1 p2 a b = b'
  where (b', _) = reflect1 p1 p2 c a 1.0 b
        c = Condition 0.5 0.5 0 -- neutral condition
