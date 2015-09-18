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
-- and determines the most likely scenarios that the wain could be
-- facing.
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
    _defaultOutcomes,
    defaultOutcomes,
    depth,
    _depth,
    generateResponses
  ) where

import ALife.Creatur.Wain.MuserInternal
