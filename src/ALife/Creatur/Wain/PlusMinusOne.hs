------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PlusMinusOne
-- Copyright   :  (c) Amy de Buitléir 2013-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Numbers on the interval -1 to 1, inclusive.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
module ALife.Creatur.Wain.PlusMinusOne
  (
    interval,
    PM1Double,
    pm1ToDouble,
    doubleToPM1,
    forceDoubleToPM1,
    pm1Apply,
    pm1Diff,
    adjustPM1Double,
    pm1VectorDiff,
    adjustPM1Vector,
    adjustPM1VectorPreserveLength,
    pm1DoublesTo8BitHex,
    pm1DoubleTo8BitHex
  ) where

-- TODO: Find some way to eliminate replicated code between this
-- module and UIDouble. May need to use Template Haskell.

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import           ALife.Creatur.Genetics.Diploid
    (Diploid, express)
import           ALife.Creatur.Wain.Pretty
    (Pretty (..))
import           ALife.Creatur.Wain.UnitInterval
    (UIDouble, doubleToUI, uiToDouble)
import           ALife.Creatur.Wain.Util
    (enforceRange, inRange, scaleFromWord64, scaleToWord64, scaleToWord8)
import           Control.DeepSeq
    (NFData)
import           Data.Datamining.Pattern
    (adjustNum)
import           Data.List
    (intercalate)
import           Data.Serialize
    (Serialize)
import           GHC.Generics
    (Generic)
import           System.Random
    (Random (..), randomR)
import           Text.Printf
    (printf)
import           Text.Read
    (readPrec)

-- | The interval -1 to 1, inclusive.
interval :: (Double, Double)
interval = (-1, 1)

-- | A number on the interval -1 to 1, inclusive.
newtype PM1Double = PM1Double Double
  deriving (Eq, Ord, Generic, Serialize, NFData)

-- | Extract the value from a @PM1Double@.
pm1ToDouble :: PM1Double -> Double
pm1ToDouble (PM1Double a) = a

-- | Convert a value to a @PM1Double@.
--   If the value is outside the allowed interval, an error will be thrown.
doubleToPM1 :: Double -> PM1Double
doubleToPM1 x = if (-1) <= x && x <= 1
                  then PM1Double x
                  else error $ "value " ++ show x ++ " not in (-1,1)"

-- | Convert a value to a @PM1Double@. The value will be capped to the
--   allowed interval.
forceDoubleToPM1 :: Double -> PM1Double
forceDoubleToPM1 = doubleToPM1 . max (-1) . min 1

-- | Apply a function to a value in the allowed interval.
pm1Apply :: (Double -> Double) -> PM1Double -> PM1Double
pm1Apply f (PM1Double x) = doubleToPM1 (f x)

-- | @'adjustPM1Double' target r pattern@ returns a modified copy
  --   of @pattern@ that is more similar to @target@ than @pattern@ is.
  --   The magnitude of the adjustment is controlled by the @r@
  --   parameter, which should be a number between 0 and 1. Larger
  --   values for @r@ permit greater adjustments. If @r@=1,
  --   the result should be identical to the @target@. If @r@=0,
  --   the result should be the unmodified @pattern@.
adjustPM1Double :: PM1Double -> UIDouble -> PM1Double -> PM1Double
adjustPM1Double (PM1Double target) r (PM1Double x)
  | inRange interval x' = doubleToPM1 x'
  | otherwise           = error $ "adjustPM1Double: out of bounds"
                            ++ " target=" ++ show target
                            ++ " r=" ++ show r
                            ++ " x=" ++ show x
  where x' = adjustNum target (uiToDouble r) x

instance Show PM1Double where
  show (PM1Double a) = show a

instance Read PM1Double where
  readPrec = fmap doubleToPM1 readPrec

instance Num PM1Double where
 (+) (PM1Double x) (PM1Double y) = doubleToPM1 (x + y)
 (-) (PM1Double x) (PM1Double y) = doubleToPM1 (x - y)
 (*) (PM1Double x) (PM1Double y) = doubleToPM1 (x * y)
 abs (PM1Double x) = doubleToPM1 (abs x)
 signum (PM1Double x) = doubleToPM1 (signum x)
 fromInteger = doubleToPM1 . fromInteger
 negate (PM1Double x) = PM1Double (-x)

instance Fractional PM1Double where
  (/) (PM1Double x) (PM1Double y) = doubleToPM1 (x / y)
  fromRational = doubleToPM1 . fromRational

instance Floating PM1Double where
  pi = doubleToPM1 pi
  exp = doubleToPM1 . exp . pm1ToDouble
  log = doubleToPM1 . log . pm1ToDouble
  sqrt = doubleToPM1 . sqrt . pm1ToDouble
  (**) (PM1Double x) (PM1Double y) = doubleToPM1 (x ** y)
  sin = doubleToPM1 . sin . pm1ToDouble
  cos = doubleToPM1 . cos . pm1ToDouble
  tan = doubleToPM1 . tan . pm1ToDouble
  asin = doubleToPM1 . asin . pm1ToDouble
  acos = doubleToPM1 . acos . pm1ToDouble
  atan = doubleToPM1 . atan . pm1ToDouble
  sinh = doubleToPM1 . sinh . pm1ToDouble
  cosh = doubleToPM1 . cosh . pm1ToDouble
  tanh = doubleToPM1 . tanh . pm1ToDouble
  asinh = doubleToPM1 . asinh . pm1ToDouble
  acosh = doubleToPM1 . acosh . pm1ToDouble
  atanh = doubleToPM1 . atanh . pm1ToDouble

instance Real PM1Double where
  toRational (PM1Double x) = toRational x

-- | The initial sequences stored at birth are genetically determined.
instance W8.Genetic PM1Double where
  put x = W8.put . scaleToWord64 interval . enforceRange interval
                    . pm1ToDouble $ x
  get = fmap (fmap (PM1Double . scaleFromWord64 interval)) W8.get

instance Diploid PM1Double where
  express (PM1Double x) (PM1Double y) = PM1Double $ (x + y)/2

instance Random PM1Double where
  randomR (PM1Double a, PM1Double b) g = (PM1Double x, g')
    where (x, g') = randomR (a,b) g
  random = f <$> randomR (0,1)
    where f (x, y) = (doubleToPM1 x, y)

instance Pretty PM1Double where
  pretty (PM1Double x) = pretty x

-- | Returns a number between 0 and 1 which indicates how different
--   the two inputs are. A result of 0 indicates that the
--   inputs are identical.
pm1Diff :: PM1Double -> PM1Double -> UIDouble
pm1Diff (PM1Double x) (PM1Double y) = doubleToUI $ abs (x - y)/2

-- | Returns a number between 0 and 1 which indicates how different
--   the two input vectors are. A result of 0 indicates that the
--   inputs are identical.
pm1VectorDiff :: [PM1Double] -> [PM1Double] -> UIDouble
pm1VectorDiff xs ys
  | null xs && null ys     = doubleToUI 0
  | null xs || null ys     = doubleToUI 1
  | inRange interval diff = doubleToUI diff
  | otherwise             = error $ "pm1VectorDiff: out of bounds"
                               ++ " xs=" ++ show xs
                               ++ " ys=" ++ show ys
  where diff = d / fromIntegral (length deltas)
        deltas = zipWith pm1Diff xs ys
        d = sum $ map uiToDouble deltas

-- | @'adjustVector' target amount vector@ adjusts each element of
--   @vector@ to move it closer to the corresponding element of
--   @target@.
--   The amount of adjustment is controlled by the learning rate
--   @amount@, which is a number between 0 and 1.
--   Larger values of @amount@ permit more adjustment.
--   If @amount@=1, the result will be identical to the @target@.
--   If @amount@=0, the result will be the unmodified @pattern@.
--   If @target@ is shorter than @vector@, the result will be the same
--   length as @target@.
--   If @target@ is longer than @vector@, the result will be the same
--   length as @vector@.
adjustPM1Vector :: [PM1Double] -> UIDouble -> [PM1Double] -> [PM1Double]
adjustPM1Vector ts r = zipWith (`adjustPM1Double` r) ts

-- | Same as @'adjustPM1Vector'@, except that the result will always be
--   the same length as @vector@.
--   This means that if @target@ is shorter than @vector@, the
--   "leftover" elements of @vector@ will be copied the result,
--   unmodified.
adjustPM1VectorPreserveLength
  :: [PM1Double] -> UIDouble -> [PM1Double] -> [PM1Double]
adjustPM1VectorPreserveLength _ _ [] = []
adjustPM1VectorPreserveLength [] _ x = x
adjustPM1VectorPreserveLength (t:ts) r (x:xs)
  = adjustPM1Double t r x : adjustPM1VectorPreserveLength ts r xs

-- | Given a sequence of numbers on the allowed interval], scales them
--   to the interval [0,255] and returns a hexadecimal representation.
pm1DoublesTo8BitHex :: [PM1Double] -> String
pm1DoublesTo8BitHex = intercalate ":" . map pm1DoubleTo8BitHex

-- | Given a number on the allowed interval, scales it to the interval
--   [0,255] and returns a hexadecimal representation.
pm1DoubleTo8BitHex :: PM1Double -> String
pm1DoubleTo8BitHex
  = printf "%.2X" . scaleToWord8 interval . pm1ToDouble

