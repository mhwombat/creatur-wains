------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Pretty
-- Copyright   :  (c) Amy de Buitléir 2012-2017
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

import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Map.Strict as MS
import Text.Printf (printf)
import Numeric (showHex)

-- | Values that have a pretty format.
class Pretty a where
  -- | Formats a value to make it easier for humans to read.
  pretty :: a -> String

  default pretty :: (Show a) => a -> String
  pretty = show

instance Pretty Int

instance Pretty Double where
  pretty x = if -10 < x && x < 10
               then printf "%.3f" x
               else printf "%.3g" x

instance Pretty Word8 where
  pretty = flip showHex ""

instance Pretty Word16 where
  pretty = flip showHex ""

instance Pretty Word32 where
  pretty = flip showHex ""

instance Pretty Word64 where
  pretty = flip showHex ""

instance Pretty Char
instance (Show k, Show v) => Pretty (MS.Map k v)

instance (Pretty a) => Pretty [a] where
  pretty [] = ""
  pretty [x] = pretty x
  pretty (x:xs) = pretty x ++ ',' : pretty xs

instance (Pretty a) => Pretty (Maybe a) where
  pretty (Just x) = pretty x
  pretty _ = "ø"

