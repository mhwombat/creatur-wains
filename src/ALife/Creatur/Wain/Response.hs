------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Response
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A potential or actual response by a wain to a stimulus.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module ALife.Creatur.Wain.Response
  (
    Response(..),
    copyOutcomesTo,
    labelSimilarity,
    addToOutcomes,
    sizedArbResponse,
    arbResponse
  ) where

import           ALife.Creatur.Wain.ResponseInternal
