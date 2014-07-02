------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Brain
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleContexts,
    StandaloneDeriving #-}
module ALife.Creatur.Wain.Brain
  (
    -- * Constructors
    Brain(..),
    buildBrain,
    -- * Learning about the objects in the universe
    classify,
    learnLabel,
    -- * Making decisions
    chooseAction,
    reflect,
    imprint,
    -- * Miscellaneous
    knownActions,
    brainOK
  ) where

import ALife.Creatur.Wain.BrainInternal
