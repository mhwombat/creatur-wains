------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain
-- Copyright   :  (c) Amy de Buitléir 2012-2019
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
    -- * Wain lenses
    name,
    appearance,
    brain,
    devotion,
    ageOfMaturity,
    passionDelta,
    boredomDelta,
    energy,
    passion,
    boredom,
    age,
    litter,
    childrenBorneLifetime,
    childrenWeanedLifetime,
    genome,
    -- * Other wain characteristics
    happiness,
    hasLitter,
    litterSize,
    childEnergy,
    Condition,
    condition,
    adjustEnergy,
    adjustBoredom,
    autoAdjustPassion,
    autoAdjustBoredom,
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
