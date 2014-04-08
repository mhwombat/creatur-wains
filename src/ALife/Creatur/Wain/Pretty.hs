------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Pretty
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Format values to make them easier for humans to read.
--
------------------------------------------------------------------------
{-# LANGUAGE DefaultSignatures #-}
module ALife.Creatur.Wain.Pretty
  (
    Pretty(..)
  ) where

import Data.Word (Word8, Word16)
import Text.Printf (printf)
import Numeric (showHex)

class Pretty a where
  pretty :: a -> String

  default pretty :: (Show a) => a -> String
  pretty = show

instance Pretty Int

instance Pretty Double where
  pretty = printf "%.3g"

instance Pretty Word8 where
  pretty = flip showHex ""

instance Pretty Word16 where
  pretty = flip showHex ""

instance (Pretty a) => Pretty [a] where
  pretty [] = ""
  pretty [x] = pretty x
  pretty (x:xs) = pretty x ++ ',' : pretty xs

instance (Pretty a) => Pretty (Maybe a) where
  pretty (Just x) = pretty x
  pretty _ = "ø"

