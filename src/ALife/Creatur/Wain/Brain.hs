------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Brain
-- Copyright   :  (c) Amy de Buitléir 2012-2015
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
    Brain(..),
    -- * Lenses
    classifier,
    predictor,
    -- * Making decisions
    chooseAction,
    reflect,
    imprint,
    -- * Miscellaneous
    happiness,
    brainOK
  ) where

import ALife.Creatur.Wain.BrainInternal
