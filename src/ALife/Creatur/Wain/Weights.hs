------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Weights
-- Copyright   :  (c) Amy de Buitléir 2015
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
    toDoubles,
    weightAt
  ) where

import ALife.Creatur.Wain.WeightsInternal
