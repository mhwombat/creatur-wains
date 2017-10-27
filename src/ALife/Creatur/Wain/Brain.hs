------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Brain
-- Copyright   :  (c) Amy de Buitléir 2012-2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module ALife.Creatur.Wain.Brain
  (
    -- * Constructors
    Brain,
    makeBrain,
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
    imprintOutcomes,
    actionCounts,
    -- * Making decisions
    chooseAction,
    reflect,
    imprint,
    -- * Miscellaneous
    happiness,
    decisionQuality,
    scenarioReport,
    responseReport,
    decisionReport
  ) where

import ALife.Creatur.Wain.BrainInternal
