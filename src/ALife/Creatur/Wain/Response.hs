------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Response
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A potential or actual response by a wain to a stimulus.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module ALife.Creatur.Wain.Response
  (
    Response(..),
    scenario,
    action,
    outcome,
    copyOutcomeTo,
    setOutcome,
    randomResponse,
    similarityIgnoringOutcome
  ) where

import ALife.Creatur.Wain.ResponseInternal
