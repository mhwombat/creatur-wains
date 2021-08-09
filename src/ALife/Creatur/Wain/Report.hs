------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Report
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Generate reports about objects.
--
------------------------------------------------------------------------
{-# LANGUAGE DefaultSignatures #-}
module ALife.Creatur.Wain.Report
  (
    Report(..)
  ) where

-- | Objects for which a report can be generated.
class Report a where
  -- | Generate a report for the object.
  report :: a -> [String]
