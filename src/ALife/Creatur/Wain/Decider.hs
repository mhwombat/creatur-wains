------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Decider
-- Copyright   :  (c) Amy de Buitléir 2013-2014
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
    possibleActions,
    deciderOK
  ) where

import ALife.Creatur.Wain.GeneticSOM (GeneticSOM, Label, justClassify,
  models, modelAt, somOK)
import ALife.Creatur.Wain.Response (Response(..), action,
  similarityIgnoringOutcome)
import ALife.Creatur.Wain.Scenario (Scenario)
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
        rawOutcome = fromJust . outcome $ model
        adjustedOutcome
          = Just $ similarityIgnoringOutcome model r * rawOutcome
        r3 = r { outcome=adjustedOutcome }

-- -- | Predicts the outcome of a response based on the decider models,
-- --   and updates the outcome field in that response.
-- predict :: (Eq a) => Decider a -> Scenario -> a -> Response a
-- predict d s a = r { outcome=Just o }
--   where r = Response s a Nothing
--         applicableModels = filter (\x -> action x == a) $ models d
--         similarities
--           = map (similarityIgnoringOutcome r) applicableModels
--         rawOutcomes = map (fromJust . outcome) applicableModels
--         adjustedOutcomes = zipWith (*) similarities rawOutcomes
--         o = (sum adjustedOutcomes)/(sum similarities)

-- | Returns the list of actions that this decider "knows".
possibleActions :: (Eq a) => Decider a -> [a]
possibleActions = nub . map action . models 

-- | Returns @True@ if the SOM has a valid Exponential and at least one
--   model, and all models have predicted results; returns @False@
--   otherwise.
deciderOK :: Eq a => Decider a -> Bool
deciderOK d = somOK d && all modelOK (models d)

modelOK :: Response a -> Bool
modelOK = isJust . outcome
