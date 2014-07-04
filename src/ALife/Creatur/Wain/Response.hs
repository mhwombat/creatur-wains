------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Response
-- Copyright   :  (c) Amy de Buitléir 2013-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A potential or actual response by a wain to a stimulus.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleContexts #-}
module ALife.Creatur.Wain.Response
  (
    Response(..),
    copyOutcomeTo,
    setOutcome,
    randomResponse,
    similarityIgnoringOutcome
  ) where

import ALife.Creatur.Wain.ResponseInternal
