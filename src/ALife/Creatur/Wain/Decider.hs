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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module ALife.Creatur.Wain.Decider
  (
    Label,
    Decider,
    predict,
    knownActions,
    imprint,
    deciderOK
  ) where

import ALife.Creatur.Wain.GeneticSOM (GeneticSOM, Label, justClassify,
  models, modelAt, somOK, reportAndTrain)
import ALife.Creatur.Wain.Response (Response(..), outcome,
  similarityIgnoringOutcome, setOutcome)
import ALife.Creatur.Wain.Scenario (Scenario)
import Control.Lens
import Data.List (nub)
import Data.Maybe (fromJust, isJust)

type Decider a = GeneticSOM (Response a)

-- | Predicts the outcome of a response based on the decider models,
--   and updates the outcome field in that response.
predict :: (Eq a) => Decider a -> Scenario -> a -> (Response a, Label)
predict d s a = (r3, k)
  where r = Response s a Nothing
        k = justClassify d r
        model = d `modelAt` k
        rawOutcome = fromJust . _outcome $ model
        adjustedOutcome
          = Just $ similarityIgnoringOutcome model r * rawOutcome
        r3 = set outcome adjustedOutcome r

-- | Teaches a response to the decider.
imprint :: (Eq a) => Decider a -> Scenario -> a -> Decider a
imprint d s a =
  if a `elem` knownActions d
    then d'
    else d -- Can't teach this response
  where (r, _) = predict d s a
        r' = r `setOutcome` 1
        (_, _, d') = reportAndTrain d r'

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
