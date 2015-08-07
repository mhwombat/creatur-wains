------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Muser
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- The muser takes classification information (labels and
-- probabilities) about the objects in a wain's field of "view",
-- and determines the scenarios that the wain is likely facing.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ALife.Creatur.Wain.Muser
  (
    Muser,
    makeMuser,
    muserOK,
    _defaultOutcome,
    defaultOutcome,
    depth,
    _depth,
    generateResponses
  ) where

import ALife.Creatur.Wain.MuserInternal
