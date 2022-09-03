------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.LearningParams
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Learning parameters for self-organising maps.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.LearningParams
  (
    LPI.LearningParams,
    LPI.mkLearningParams,
    LPI.toLearningFunction,
    LPI.LearningParamRanges(..),
    LPI.widestLearningParamRanges,
    LPI.randomLearningParams,
  ) where

import ALife.Creatur.Wain.LearningParamsInternal qualified as LPI
