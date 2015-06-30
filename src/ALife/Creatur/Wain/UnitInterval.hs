------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.UnitInterval
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Numbers on the unit interval (0 to 1, inclusive).
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module ALife.Creatur.Wain.UnitInterval
  (
    interval,
    UIDouble,
    uiToDouble,
    doubleToUI,
    forceDoubleToUI,
    uiApply,
    uiDiff,
    adjustUIDouble,
    uiVectorDiff,
    adjustUIVector,
    adjustUIVectorPreserveLength,
    uiDoublesTo8BitHex,
    uiDoubleTo8BitHex
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Wain.Util (inRange, enforceRange,
  scaleToWord8, scaleFromWord8)
import Control.DeepSeq (NFData)
import Data.Datamining.Pattern (adjustNum)
import Data.List (intercalate)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Random (Random(..), randomR)
import Text.Printf (printf)
import Text.Read (readPrec)

interval :: (Double, Double)
interval = (0, 1)

-- | A number on the unit interval 0 to 1, inclusive.
newtype UIDouble = UIDouble Double
  deriving (Eq, Ord, Generic, Serialize, NFData)

-- | Extract the value from a @UIDouble@.
uiToDouble :: UIDouble -> Double
uiToDouble (UIDouble a) = a

-- | Convert a value to a @UIDouble@.
--   If the value is outside the unit interval, an error will be thrown.
doubleToUI :: Double -> UIDouble
doubleToUI x = if inRange interval x
                 then UIDouble x
                 else error $ "value " ++ show x ++ " not in (0,1)"

-- | Convert a value to a @UIDouble@. The value will be capped to the
--   unit interval.
forceDoubleToUI :: Double -> UIDouble
forceDoubleToUI = doubleToUI . max 0 . min 1

-- | Apply a function to a value in the unit interval.
uiApply :: (Double -> Double) -> UIDouble -> UIDouble
uiApply f (UIDouble x) = doubleToUI (f x)

adjustUIDouble :: UIDouble -> UIDouble -> UIDouble -> UIDouble
adjustUIDouble (UIDouble target) (UIDouble r) (UIDouble x)
  = doubleToUI $ adjustNum target r x

instance Show UIDouble where
  show (UIDouble a) = show a

instance Read UIDouble where
  readPrec = fmap doubleToUI readPrec

instance Num UIDouble where
 (+) (UIDouble x) (UIDouble y) = doubleToUI (x + y)
 (-) (UIDouble x) (UIDouble y) = doubleToUI (x - y)
 (*) (UIDouble x) (UIDouble y) = doubleToUI (x * y)
 abs x = x
 signum (UIDouble x) = doubleToUI (signum x)
 fromInteger = doubleToUI . fromInteger
 negate (UIDouble 0) = UIDouble 0
 negate _ = error "value not in unit interval"

instance Fractional UIDouble where
  (/) (UIDouble x) (UIDouble y) = doubleToUI (x / y)
  fromRational = doubleToUI . fromRational

instance Floating UIDouble where
  pi = doubleToUI pi
  exp = doubleToUI . exp . uiToDouble
  log = doubleToUI . log . uiToDouble
  sqrt = doubleToUI . sqrt . uiToDouble
  (**) (UIDouble x) (UIDouble y) = doubleToUI (x ** y)
  sin = doubleToUI . sin . uiToDouble
  cos = doubleToUI . cos . uiToDouble
  tan = doubleToUI . tan . uiToDouble
  asin = doubleToUI . asin . uiToDouble
  acos = doubleToUI . acos . uiToDouble
  atan = doubleToUI . atan . uiToDouble
  sinh = doubleToUI . sinh . uiToDouble
  cosh = doubleToUI . cosh . uiToDouble
  tanh = doubleToUI . tanh . uiToDouble
  asinh = doubleToUI . asinh . uiToDouble
  acosh = doubleToUI . acosh . uiToDouble
  atanh = doubleToUI . atanh . uiToDouble

instance Real UIDouble where
  toRational (UIDouble x) = toRational x

-- | The initial sequences stored at birth are genetically determined.
instance W8.Genetic UIDouble where
  put = W8.put . scaleToWord8 interval . enforceRange interval
          . uiToDouble
  get = fmap (fmap (UIDouble . scaleFromWord8 interval)) W8.get

instance Diploid UIDouble where
  express (UIDouble x) (UIDouble y) = UIDouble $ (x + y)/2
  
instance Random UIDouble where
  randomR (UIDouble a, UIDouble b) g = (UIDouble x, g')
    where (x, g') = randomR (a,b) g
  random = f <$> randomR (0,1)
    where f (x, y) = (doubleToUI x, y)

uiDiff :: UIDouble -> UIDouble -> UIDouble
uiDiff (UIDouble x) (UIDouble y) = doubleToUI $ abs (x - y)

uiVectorDiff :: [UIDouble] -> [UIDouble] -> UIDouble
uiVectorDiff xs ys
  | null xs && null ys = UIDouble 0
  | null xs || null ys = UIDouble 1
  | otherwise         = doubleToUI $ d / fromIntegral (length deltas)
  where deltas = zipWith uiDiff xs ys
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
adjustUIVector :: [UIDouble] -> UIDouble -> [UIDouble] -> [UIDouble]
adjustUIVector ts r = zipWith (`adjustUIDouble` r) ts

-- | Same as @'adjustUIVector'@, except that the result will always be
--   the same length as @vector@.
--   This means that if @target@ is shorter than @vector@, the
--   "leftover" elements of @vector@ will be copied the result,
--   unmodified.
adjustUIVectorPreserveLength
  :: [UIDouble] -> UIDouble -> [UIDouble] -> [UIDouble]
adjustUIVectorPreserveLength _ _ [] = []
adjustUIVectorPreserveLength [] _ x = x
adjustUIVectorPreserveLength (t:ts) r (x:xs)
  = adjustUIDouble t r x : adjustUIVectorPreserveLength ts r xs

-- | Given a sequence of numbers on the unit interval], scales them
--   to the interval [0,255] and returns a hexadecimal representation.
uiDoublesTo8BitHex :: [UIDouble] -> String
uiDoublesTo8BitHex = intercalate ":" . map uiDoubleTo8BitHex

-- | Given a number on the unit interval, scales it to the interval
--   [0,255] and returns a hexadecimal representation.
uiDoubleTo8BitHex :: UIDouble -> String
uiDoubleTo8BitHex
  = printf "%.2X" . scaleToWord8 interval . uiToDouble

