------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain
  (
    -- * Wains
    Wain(..),
    buildWainAndGenerateGenome,
    -- * Other wain characteristics
    happiness,
    hasLitter,
    litterSize,
    childEnergy,
    Condition,
    condition,
    adjustEnergy,
    autoAdjustPassion,
    coolPassion,
    incAge,
    prettyClassifierModels,
    prettyPredictorModels,
    prettyBrainSummary,
    -- * Reacting to stimuli
    DecisionReport,
    novelties,
    adjNovelties,
    chooseAction,
    prettyClassificationReport,
    prettyScenarioReport,
    prettyPredictionReport,
    prettyActionReport,
    -- * Learning through experience
    ReflectionReport,
    prettyReflectionReport,
    reflect,
    happinessError,
    -- * Learning through instruction
    StimulusImprintReport,
    prettyStimulusImprintReport,
    imprintStimulus,
    ResponseImprintReport,
    prettyResponseImprintReport,
    imprintResponse,
    -- * Reproduction
    mate,
    weanMatureChildren,
    pruneDeadChildren,
    -- * Misc.
    Label,
    packageVersion
  ) where

import           ALife.Creatur.Wain.Brain      (Condition)
import           ALife.Creatur.Wain.GeneticSOM (Label)
import           ALife.Creatur.WainInternal
