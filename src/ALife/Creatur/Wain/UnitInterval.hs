------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UnitInterval
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions that don't fit anywhere else.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ALife.Creatur.Wain.UnitInterval
  (
    UIDouble,
    uiToDouble,
    doubleToUI,
    uiApply,
    vectorDiff
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Util (unitInterval, enforceRange,
  scaleToWord8, scaleFromWord8)
-- import Data.Datamining.Pattern (adjustNum, adjustVector)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Random (Random(..), randomR)
import Text.Read (readPrec)

-- | A number on the unit interval 0 to 1, inclusive.
--   In fact, this type can hold any Double value for convenience in
--   calculations. (E.g., an intermediate value in a computation may
--   be outside the unit interval.) However, if the value is stored
--   in the genome, it will be forced into the unit interval first.
--   The functions random and arbitrary will only generate values in
--   the unit interval.
newtype UIDouble = UIDouble Double
  deriving (Eq, Ord, Generic, Enum, Num, Fractional, Floating, RealFrac)

-- | Extract the value from a @UIDouble@.
--   Note that this value can be outside the unit interval; see
--   @UIDouble@ for more information.
uiToDouble :: UIDouble -> Double
uiToDouble (UIDouble a) = a

-- | Convert a value to a @UIDouble@. The value will be capped to the
--   unit interval.
doubleToUI :: Double -> UIDouble
doubleToUI = UIDouble . enforceRange unitInterval

-- | Apply a function to a value in the unit interval.
uiApply :: (Double -> Double) -> UIDouble -> UIDouble
uiApply f (UIDouble x) = doubleToUI (f x)

instance Show UIDouble where
  show (UIDouble a) = show a

instance Read UIDouble where
  readPrec = fmap doubleToUI readPrec

instance Real UIDouble where
  toRational (UIDouble a) = toRational a

-- | The initial sequences stored at birth are genetically determined.
instance W8.Genetic UIDouble where
  put = W8.put . scaleToWord8 unitInterval . enforceRange unitInterval . uiToDouble
  get = fmap (fmap (UIDouble . scaleFromWord8 unitInterval)) W8.get

instance Serialize UIDouble
instance Diploid UIDouble

instance Random UIDouble where
  randomR (UIDouble a, UIDouble b) g = (UIDouble x, g')
    where (x, g') = randomR (a,b) g
  random = randomR (0,1)

-- We're scaling the euclidean distance by the length of the vector
vectorDiff :: Fractional a => [a] -> [a] -> a
vectorDiff xs ys
  | null xs && null ys = 0
  | null xs || null ys = 1
  | otherwise         = d / fromIntegral (length deltas)
  where deltas = zipWith (-) xs ys
        d = sum $ map (\x -> x*x) deltas
