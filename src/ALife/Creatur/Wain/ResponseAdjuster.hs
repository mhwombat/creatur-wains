------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ResponseAdjuster
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Learning functions for self-organising maps.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
module ALife.Creatur.Wain.ResponseAdjuster
  (
    ResponseAdjuster
  ) where

import           ALife.Creatur.Wain.PatternAdjuster (PatternAdjuster)
import           ALife.Creatur.Wain.Response        (Response)

type ResponseAdjuster a = PatternAdjuster (Response a)
