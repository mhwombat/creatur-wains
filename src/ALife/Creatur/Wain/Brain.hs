------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Brain
-- Copyright   :  (c) Amy de Buitléir 2012-2018
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
    ImprintReport(..),
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
    imprint,
    -- * Miscellaneous
    happiness,
    decisionQuality,
    prettyScenarioReport,
    prettyActionReport,
    prettyReflectionReport,
    prettyImprintReport
  ) where

import           ALife.Creatur.Wain.BrainInternal
import           ALife.Creatur.Wain.Classifier
