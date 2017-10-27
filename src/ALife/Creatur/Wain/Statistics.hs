------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Statistics
-- Copyright   :  (c) Amy de Buitléir 2013-2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Statistical calculations
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module ALife.Creatur.Wain.Statistics
  (
    Statistical,
    Statistic,
    name,
    prefix,
    apply,
    stats,
    dStat,
    iStat,
    summarise,
    lookup,
    mean,
    popVariance,
    popStdDev
  ) where

import Prelude hiding (lookup)
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.Raw (Raw, raw)
import Data.List (transpose)
import Data.Serialize (Serialize)
import GHC.Generics

-- | A value for calculating statistics with.
data Statistic = DStatistic {sName :: String, sVal :: Double}
  | IStatistic {sName :: String, sVal :: Double}
  deriving (Eq, Generic, Serialize)

instance Show Statistic where
  show (DStatistic s x) = "dStat \"" ++ s ++ "\" " ++ show x
  show (IStatistic s x) = "iStat \"" ++ s ++ "\" " ++ show (round x :: Int)

instance Pretty Statistic where
  pretty (DStatistic s x) = s ++ "=" ++ pretty x
  pretty (IStatistic s x) = s ++ "=" ++ pretty (round x :: Int)

instance Raw Statistic where
  raw (DStatistic s x) = s ++ "=" ++ raw x
  raw (IStatistic s x) = s ++ "=" ++ raw x

-- | Creates a value that will be displayed as a double.
dStat :: Real a => String -> a -> Statistic
dStat s = DStatistic s . realToFrac

-- | Creates a value that will be displayed as an integer.
iStat :: Integral a => String -> a -> Statistic
iStat s v = IStatistic s (fromIntegral v)

toDStat :: Statistic -> Statistic
toDStat (IStatistic s x) = DStatistic s x
toDStat x = x

-- | Prefixes the given string to the value's "name".
prefix :: String -> Statistic -> Statistic
prefix s x = x { sName = s ++ sName x }

-- | Applies a function to a value.
apply :: (Double -> Double) -> Statistic -> Statistic
apply f x = x { sVal = f (sVal x) }

-- | Returns the value's "name".
name :: Statistic -> String
name = sName

-- | Typeclass for values that we can calculate statistics on.
class Statistical a where
  stats :: a -> [Statistic]

-- | Given a 2-dimensional table of values, reports some statistics.
summarise :: [[Statistic]] -> [[Statistic]]
summarise xss = [maxima,minima,averages,stdDevs,sums]
  where yss = transpose xss
        maxima   = compile "max. " maximum yss
        minima   = compile "min. " minimum yss
        averages = compile "avg. " mean $ map (map toDStat) yss
        stdDevs  = compile "std. dev. " popStdDev $
                     map (map toDStat) yss
        sums     = compile "total " sum yss

compile :: String -> ([Double] -> Double) -> [[Statistic]] -> [Statistic]
compile s f yss = map (prefix s) $ applyToColumns f yss

applyToColumns :: ([Double] -> Double) -> [[Statistic]] -> [Statistic]
applyToColumns f = map (applyToColumn f)

applyToColumn :: ([Double] -> Double) -> [Statistic] -> Statistic
applyToColumn f xs@(y:_) = y { sVal=f (map sVal xs) }
applyToColumn _ [] = error "no data"

-- | Lookup a value in a set of statistics by its key.
lookup :: String -> [Statistic] -> Maybe Double
lookup _ [] = Nothing
lookup key (x:xs) = if key == sName x
                      then Just (sVal x)
                      else lookup key xs

-- | Calculate the mean of a set of values.
-- mean :: (Eq a, Fractional a, Foldable t) => t a -> a
mean :: (Fractional a, Eq a) => [a] -> a
mean xs
  | count == 0 = error "no data"
  | otherwise = total / count
  where (total, count) = foldr f (0, 0) xs
        f x (y, n) = (y+x, n+1)

-- | Calculate the population variance of a set of values.
-- popVariance :: (Eq a, Fractional a, Foldable t) => t a -> a
popVariance :: (Fractional a, Eq a) => [a] -> a
popVariance xs
  | count == 0 = error "no data"
  | otherwise = total / count
  where (total, count) = foldr f (0, 0) xs
        f x (y, n) = (y + (x - mu)*(x - mu), n+1)
        mu = mean xs

-- | Calculate the population standard deviation of a set of values.
-- popStdDev :: (Eq a, Fractional a, Floating a, Foldable t) => t a -> a
popStdDev :: [Double] -> Double
popStdDev = sqrt . popVariance
