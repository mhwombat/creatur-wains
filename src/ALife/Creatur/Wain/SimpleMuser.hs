------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SimpleMuser
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A muser that works with enumerable actions.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.SimpleMuser
  (
    SimpleMuser,
    defaultOutcomes,
    depth,
    makeMuser,
    generateResponses,
    bestHypotheses
  ) where

import           ALife.Creatur.Wain.SimpleMuserInternal
