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
    doublesTo8BitHex
  ) where

import Data.Word (Word8, Word16)
import Data.Datamining.Pattern (Pattern(..), Metric, adjustNum,
  adjustVector)
import Text.Printf (printf)

unitInterval :: Num a => (a, a)
unitInterval = (0,1)

inRange :: Ord c => (c, c) -> c -> Bool
inRange (a,b) c = c >= a && c <= b

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

forceIntToWord8 :: Int -> Word8
forceIntToWord8 = fromIntegral . min maxBound

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

forceIntToWord16 :: Int -> Word16
forceIntToWord16 = fromIntegral . min maxBound

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

doublesTo8BitHex :: [Double] -> String
doublesTo8BitHex = concatMap doubleTo8BitHex

doubleTo8BitHex :: Double -> String
doubleTo8BitHex = printf "%.2X" . scaleToWord8 unitInterval
