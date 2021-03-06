------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Brain
-- Copyright   :  (c) Amy de Buitléir 2012-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module ALife.Creatur.Wain.Brain
  (
    -- * Constructors
    Brain,
    makeBrain,
    ClassifierReport(..),
    DecisionReport(..),
    ScenarioReport,
    PredictorReport,
    ActionReport,
    ReflectionReport(..),
    Condition,
    -- * Lenses
    _classifier,
    _muser,
    _predictor,
    _happinessWeights,
    _tiebreaker,
    _imprintOutcomes,
    _actionCounts,
    classifier,
    muser,
    predictor,
    happinessWeights,
    tiebreaker,
    strictness,
    imprintOutcomes,
    reinforcementDeltas,
    actionCounts,
    -- * Reacting to a stimulus
    chooseAction,
    classifyInputs,
    reflect,
    imprintStimulus,
    imprintResponse,
    -- * Miscellaneous
    happiness,
    decisionQuality,
    prettyScenarioReport,
    prettyActionReport,
    prettyReflectionReport
  ) where

import           ALife.Creatur.Wain.BrainInternal
import           ALife.Creatur.Wain.Classifier
