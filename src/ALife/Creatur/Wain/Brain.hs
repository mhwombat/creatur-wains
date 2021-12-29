------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Brain
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
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
    Brain(..),
    makeBrain,
    ClassifierReport(..),
    DecisionReport(..),
    ScenarioReport,
    PredictorReport,
    ActionReport,
    ReflectionReport(..),
    Condition,
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
