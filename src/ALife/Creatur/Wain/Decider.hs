------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Decider
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A decision-maker based on a Kohonen Self-organising Map (SOM).
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module ALife.Creatur.Wain.Decider
  (
    Label,
    Decider,
    DeciderThinker(..),
    buildDecider,
    predict,
    knownActions,
    imprint,
    deciderOK,
    deciderQuality
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (GeneticSOM, ExponentialParams(..),
  Label, Thinker(..), buildGeneticSOM, justClassify, models, modelAt,
  somOK, reportAndTrain, teacher, discrimination, counterMap)
import ALife.Creatur.Wain.Response (Response(..), outcome,
  diffIgnoringOutcome, similarityIgnoringOutcome, setOutcome,
  makeResponseSimilar, action)
import ALife.Creatur.Wain.Scenario (Scenario)
import ALife.Creatur.Wain.Weights (Weights)
import Control.Lens
import Data.Function (on)
import Data.List (nub, groupBy, sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- | @'DeciderThinker' cs sw rw@ constructs an object which is
--   responsible for comparing and adjusting response patterns.
--   The parameter @cw@ determines the relative weight to assign to
--   differences in energy, passion, and whether or not there is a
--   litter when comparing two patterns.
--   The parameter @sw@ determines the relative weight to assign to
--   differences between each corresponding pair of objects represented
--   by the scenario when comparing two patterns.
--   The parameter @rw@ determines the relative weight to assign to
--   differences in the scenarios and the outcomes when comparing two
--   patterns.
data DeciderThinker a = DeciderThinker Weights Weights Weights
  deriving (Eq, Show, Generic)

instance (Eq a) => Thinker (DeciderThinker a) where
  type Pattern (DeciderThinker a) = Response a
  diff (DeciderThinker cw sw rw) = diffIgnoringOutcome cw sw rw
  adjust _ = makeResponseSimilar

instance Serialize (DeciderThinker a)
instance Genetic (DeciderThinker a)
instance Diploid (DeciderThinker a)

type Decider a = GeneticSOM (Response a) (DeciderThinker a)

buildDecider
  :: Eq a
    => ExponentialParams -> Weights -> Weights -> Weights -> [Response a]
      -> Decider a
buildDecider e cw sw rw = buildGeneticSOM e (DeciderThinker cw sw rw)

-- | Predicts the outcome of a response based on the decider models,
--   and updates the outcome field in that response.
predict :: (Eq a) => Decider a -> Scenario -> a -> (Response a, Label)
predict d s a = (r3, k)
  where r = Response s a Nothing
        k = justClassify d r
        model = d `modelAt` k
        rawOutcome = fromJust . _outcome $ model
        (DeciderThinker cw sw rw)  = view teacher d
        adjustedOutcome
          = Just $ similarityIgnoringOutcome cw sw rw model r * rawOutcome
        r3 = set outcome adjustedOutcome r

-- | Teaches a response to the decider (teaches it that the response
--   increases happiness by 1).
imprint :: (Eq a) => Decider a -> Scenario -> a -> Decider a
imprint d s a =
  if a `elem` knownActions d
    then d'
    else d -- Can't teach this response
  where (r, _) = predict d s a
        r' = r `setOutcome` 1
        (_, _, _, d') = reportAndTrain d r'

-- | Returns the list of actions that this decider "knows".
knownActions :: (Eq a) => Decider a -> [a]
knownActions = nub . map _action . models 

-- | Returns @True@ if the SOM has a valid Exponential and at least one
--   model, and all models have predicted results; returns @False@
--   otherwise.
deciderOK :: Eq a => Decider a -> Bool
deciderOK d = somOK d && all modelOK (models d)

modelOK :: Response a -> Bool
modelOK = isJust . _outcome

deciderQuality :: (Eq a, Ord a) => Decider a -> Int
deciderQuality d = discrimination actionCounts
  where modelCounts = M.elems . view counterMap $ d
        actions = map (view action) $ models d
        actionCounts = map (sum . map snd)
                         . groupBy ((==) `on` fst)
                         . sortBy (compare `on` fst)
                           $ zip actions modelCounts
