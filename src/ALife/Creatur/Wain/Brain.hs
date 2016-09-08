------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Brain
-- Copyright   :  (c) Amy de Buitléir 2012-2016
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
    _predictor,
    _happinessWeights,
    _tiebreaker,
    _responseWeights,
    _decisionDiffThreshold,
    _defaultOutcomes,
    _imprintOutcomes,
    _reinforcementDeltas,
    _actionCounts,
    classifier,
    predictor,
    happinessWeights,
    tiebreaker,
    responseWeights,
    decisionDiffThreshold,
    defaultOutcomes,
    imprintOutcomes,
    reinforcementDeltas,
    actionCounts,
    -- * Making decisions
    chooseAction,
    reflect,
    imprint,
    -- * Miscellaneous
    happiness,
    decisionQuality,
    decisionReport
  ) where

import ALife.Creatur.Wain.BrainInternal
