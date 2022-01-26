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
    novelties,
    adjNovelties,
    chooseAction,
    -- * Learning through experience
    reflect,
    happinessError,
    -- * Learning through instruction
    -- imprintStimulus,
    imprintResponse,
    -- * Reproduction
    mate,
    weanMatureChildren,
    pruneDeadChildren,
    -- * Replaying events in a wain's life
    clone,
    Event,
    runEvent,
    runLife,
    -- * Misc.
    Label,
    packageVersion
  ) where

import           ALife.Creatur.Genetics.Reproduction.Sexual (clone)
import           ALife.Creatur.Wain.Brain                   (Condition)
import           ALife.Creatur.Wain.GeneticSOM              (Label)
import           ALife.Creatur.WainInternal
import           Data.Version                               (showVersion)
import           Paths_creatur_wains                        (version)

-- | Returns the current version number of this library.
packageVersion :: String
packageVersion = "creatur-wains-" ++ showVersion version
