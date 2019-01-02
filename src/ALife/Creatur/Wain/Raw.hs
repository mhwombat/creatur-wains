------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Raw
-- Copyright   :  (c) Amy de Buitléir 2012-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Format values to make them easier to process with sed, etc.
--
------------------------------------------------------------------------
{-# LANGUAGE DefaultSignatures #-}
module ALife.Creatur.Wain.Raw
  (
    Raw(..)
  ) where

import           Data.Word
    (Word16, Word32, Word64, Word8)

-- | Values that have a "raw" format.
class Raw a where
  -- | Formats a value to make it easier to process with sed, etc.
  raw :: a -> String

  default raw :: (Show a) => a -> String
  raw = show

instance Raw Int

instance Raw Double

instance Raw Word8

instance Raw Word16

instance Raw Word32

instance Raw Word64

instance (Raw a) => Raw [a] where
  raw []     = ""
  raw [x]    = raw x
  raw (x:xs) = raw x ++ ',' : raw xs

instance (Raw a) => Raw (Maybe a) where
  raw (Just x) = raw x
  raw _        = "ø"

