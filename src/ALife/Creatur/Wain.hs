------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain
-- Copyright   :  (c) Amy de Buitléir 2012-2015
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
    -- * Constructors
    Wain(..),
    Label,
    buildWainAndGenerateGenome,
    -- * Lenses
    name,
    appearance,
    brain,
    devotion,
    ageOfMaturity,
    passionDelta,
    energy,
    passion,
    age,
    litter,
    childrenBorneLifetime,
    childrenWeanedLifetime,
    genome,
    wainSize,
    -- * Teaching and learning
    chooseAction,
    reflect,
    imprint,
    -- * Misc.
    applyMetabolismCost,
    adjustEnergy,
    adjustPassion,
    coolPassion,
    happiness,
    -- identity,
    -- appearanceOf,
    childEnergy,
    hasLitter,
    litterSize,
    incAge,
    condition,
    mate,
    weanMatureChildren,
    pruneDeadChildren,
    programVersion
  ) where

import ALife.Creatur.WainInternal
import ALife.Creatur.Wain.GeneticSOM (Label)
