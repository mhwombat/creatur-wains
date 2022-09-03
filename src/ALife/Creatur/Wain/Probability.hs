------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Probability
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Proposes hypotheses about the scenario the wain is facing, and
-- estimates the probability that each hypothesis is true.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ALife.Creatur.Wain.Probability
  (
    hypothesise,
    prettyProbability
  ) where

import ALife.Creatur.Wain.ProbabilityInternal
