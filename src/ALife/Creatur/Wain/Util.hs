------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Util
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions that don't fit anywhere else.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.Util
  (
    unitInterval,
    inRange,
    enforceRange,
    scaleToWord8,
    scaleToWord16,
    scaleFromWord8,
    scaleFromWord16,
    forceToWord8,
    scaleIntToWord8,
    scaleWord8ToInt,
    forceIntToWord8,
    word8ToInt,
    forceIntToWord16,
    word16ToInt,
    doubleTo8BitHex,
    doublesTo8BitHex,
    intersection
  ) where

import Data.Word (Word8, Word16)
import Data.Datamining.Pattern (Pattern(..), Metric, adjustNum,
  adjustVector)
import Text.Printf (printf)

-- | The range [0,1], endpoints included.
unitInterval :: Num a => (a, a)
unitInterval = (0,1)

-- | Returns @True@ if the value is in the specified range, endpoints
--   included; returns @False@ otherwise.
inRange :: Ord c => (c, c) -> c -> Bool
inRange (a,b) c = c >= a && c <= b

-- | If the value is outside the specified range, endpoints included,
--   return the nearest endpoint; otherwise, return the value.
enforceRange :: Ord c => (c, c) -> c -> c
enforceRange (a,b) = max a . min b

-- | @'scaleToWord8' (a, b) x@ scales @x@ to create a @Word8@ value,
--   such that @scaleToWord8 (a, b) a@ = 0 and
--   @scaleToWord8 (a, b) b@ = 255.
scaleToWord8 :: RealFrac a => (a, a) -> a -> Word8
scaleToWord8 (a, b) x = round (255*(x-a)/(b-a))

-- | @'scaleToWord16' (a, b) x@ scales @x@ to create a @Word16@ value,
--   such that @scaleToWord16 (a, b) a@ = 0 and
--   @scaleToWord16 (a, b) b@ = 255.
scaleToWord16 :: RealFrac a => (a, a) -> a -> Word16
scaleToWord16 (a, b) x = round (65535*(x-a)/(b-a))

-- | This function is the inverse of @'scaleToWord8'@.
--   @'scaleFromWord8' (a, b) 0@ = a and
--   @'scaleFromWord8' (a, b) 255@ = b.
scaleFromWord8 :: Fractional a => (a, a) -> Word8 -> a
scaleFromWord8 (a, b) x = a + (fromIntegral x)*(b-a)/255

-- | This function is the inverse of @'scaleToWord16'@.
--   @'scaleFromWord16' (a, b) 0@ = a and
--   @'scaleFromWord16' (a, b) 255@ = b.
scaleFromWord16 :: Fractional a => (a, a) -> Word16 -> a
scaleFromWord16 (a, b) x = a + (fromIntegral x)*(b-a)/65535

forceToWord8 :: (Num a, Ord a, RealFrac a) => a -> Word8
forceToWord8 = round . min 255

-- | @'scaleIntToWord8' (a, b) x@ scales @x@ to create a @Word8@ value,
--   such that @scaleIntToWord8 (a, b) a@ = 0 and
--   @scaleIntToWord8 (a, b) b@ = 255.
scaleIntToWord8 :: (Int, Int) -> Int -> Word8
scaleIntToWord8 (a, b) x = round (255*(x'-a')/(b'-a'))
  where a' = fromIntegral a :: Double
        b' = fromIntegral b :: Double
        x' = fromIntegral x :: Double

-- | This function is the inverse of @'scaleIntToWord8'@.
--   @'scaleWord8ToInt' (a, b) 0@ = a and
--   @'scaleWord8ToInt' (a, b) 255@ = b.
scaleWord8ToInt :: (Int, Int) -> Word8 -> Int
scaleWord8ToInt (a, b) x = a + (fromIntegral x)*(b-a) `div` 255

-- | @'forceIntToWord8' x@ returns 255 or @x@, whichever is smaller.
--   TODO: How should negative values be handled?
forceIntToWord8 :: Int -> Word8
forceIntToWord8 = fromIntegral . min maxBound

-- | Converts a @Word8@ to an @Int@.
word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

-- | @'forceIntToWord8' x@ returns 65535 or @x@, whichever is smaller.
--   TODO: How should negative values be handled?
forceIntToWord16 :: Int -> Word16
forceIntToWord16 = fromIntegral . min maxBound

-- | Converts a @Word16@ to an @Int@.
word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

instance Pattern Double where
  type Metric Double = Double
  difference a b = abs $ (-) a b
  makeSimilar = adjustNum

instance Pattern [Double] where
  type Metric [Double] = Double
  difference xs ys
    | null xs && null ys = 0
    | null xs || null ys = 1
    | otherwise         = d / (fromIntegral $ length deltas)
    where deltas = zipWith (-) xs ys
          d = sum $ map (\z -> z*z) deltas
  makeSimilar = adjustVector

-- | Given a sequence of numbers on the unit interval], scales them
--   to the interval [0,255] and returns a hexadecimal representation.
doublesTo8BitHex :: [Double] -> String
doublesTo8BitHex = concatMap doubleTo8BitHex

-- | Given a number on the unit interval, scales it to the interval
--   [0,255] and returns a hexadecimal representation.
doubleTo8BitHex :: Double -> String
doubleTo8BitHex = printf "%.2X" . scaleToWord8 unitInterval

proper :: Ord a => (a, a) -> (a, a)
proper (x, y) = if x <= y then (x, y) else (y, x)

-- | Returns the largest interval that satisfies both of the supplied
--   intervals. If either interval is reversed (i.e., the start of the
--   interval is >= the end), the endpoints will be swapped before
--   proceeding. If the supplied intervals do not overlap, the first
--   interval is used.
intersection :: Ord a => (a, a) -> (a, a) -> (a, a)
intersection (a, b) (c, d)
  | b' < c' || d' < a' = (a', b')
  | otherwise         = (max a' c', min b' d')
  where (a', b') = proper (a, b)
        (c', d') = proper (c, d)
