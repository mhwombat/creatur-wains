------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Pattern
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A pattern that can be learned.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module ALife.Creatur.Wain.Pattern
  (
    Pattern,
    diff,
    makeSimilar
  ) where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI

class Pattern p where
  diff :: p -> p -> UI.UIDouble
  makeSimilar :: p -> UI.UIDouble -> p -> p
