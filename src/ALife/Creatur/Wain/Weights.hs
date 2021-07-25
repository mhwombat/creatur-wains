------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Weights
-- Copyright   :  (c) 2015-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Weights used to calculate weighted sums.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Weights
  (
    Weights,
    makeWeights,
    weightedSum,
    weightAt,
    toUIDoubles,
    weightedUIVectorDiff,
    numWeights
  ) where

import           ALife.Creatur.Wain.WeightsInternal
