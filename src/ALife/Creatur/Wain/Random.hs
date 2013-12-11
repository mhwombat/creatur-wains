------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Random
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Random
  (
    RandomInitial(..)
  ) where

import Control.Monad.Random (Rand, RandomGen)

class RandomInitial a where
  randomInitial :: RandomGen g => Rand g a
