------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Statistician
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Proposes hypotheses about the scenario the wain is facing, and
-- estimates the probability that each hypothesis is true.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ALife.Creatur.Wain.Statistician
  (
    Probability,
    hypothesise
  ) where

import ALife.Creatur.Wain.StatisticianInternal
