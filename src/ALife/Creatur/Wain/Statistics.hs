------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Statistics
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Statistical calculations
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module ALife.Creatur.Wain.Statistics
  (
    Statistical,
    Statistic,
    name,
    value,
    prefix,
    apply,
    stats,
    dStat,
    iStat,
    dStats,
    iStats,
    kvToDStats,
    kvToIStats,
    summarise,
    lookup,
    mean,
    popVariance,
    popStdDev
  ) where

import           ALife.Creatur.Gene.Test (TestPattern(..))
import           ALife.Creatur.Gene.Numeric.Weights
    (Weights, toUIDoubles)
import           ALife.Creatur.Wain.Pretty
    (Pretty, pretty)
import           ALife.Creatur.Wain.Raw
    (Raw, raw)
import           Data.List
    (groupBy, sortOn)
import           Data.Serialize
    (Serialize)
import           GHC.Generics
import           Prelude                   hiding
    (lookup)

-- | A value for calculating statistics with.
data Statistic = DStatistic {sName :: String, sVal :: Double}
  | IStatistic {sName :: String, sVal :: Double}
  deriving (Eq, Generic, Serialize)

instance Show Statistic where
  show (DStatistic s x) = "dStat \"" ++ s ++ "\" " ++ v
    where v = if x < 0 then "(" ++ show x ++ ")"
                       else show x
  show (IStatistic s x) = "iStat \"" ++ s ++ "\" " ++ v
    where v = if x' < 0 then "(" ++ show x' ++ ")"
                       else show x'
          x' = round x :: Int

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

-- | Creates a sequence of values that will be displayed as doubles.
dStats :: Real a => String -> [a] -> [Statistic]
dStats s = map (prefix s) . kvToDStats . applyIndices

-- | Creates a sequence of value that will be displayed as integers.
iStats :: Integral a => String -> [a] -> [Statistic]
iStats s = map (prefix s) . kvToIStats . applyIndices

applyIndices :: [a] -> [(Int, a)]
applyIndices = zip [1..]

-- toDStat :: Statistic -> Statistic
-- toDStat (IStatistic s x) = DStatistic s x
-- toDStat x = x

-- | Prefixes the given string to the value's "name".
prefix :: String -> Statistic -> Statistic
prefix s x = x { sName = s ++ sName x }

-- | Applies a function to a value.
apply :: (Double -> Double) -> Statistic -> Statistic
apply f x = x { sVal = f (sVal x) }

-- | Returns the name of the statistic
name :: Statistic -> String
name = sName

-- | Returns the value of the statistic.
value :: Statistic -> Double
value = sVal

-- | Typeclass for values that we can calculate statistics on.
class Statistical a where
  stats :: a -> [Statistic]

instance (Show k, Statistical v)
    => Statistical [(k, v)] where
  stats = concatMap f
    where f (k, v) = map (prefix ("[" ++ show k ++ "]")) $ stats v

instance Statistical TestPattern where
  stats (TestPattern x) = [iStat "" x]

instance Statistical Weights where
  stats = dStats "" . toUIDoubles

kvToDStats :: (Show k, Real v) => [(k, v)] -> [Statistic]
kvToDStats = map f
    where f (k, v) = dStat (g k) v
          g k = "[" ++ show k ++ "]"

kvToIStats :: (Show k, Integral v) => [(k, v)] -> [Statistic]
kvToIStats = map f
    where f (k, v) = iStat (g k) v
          g k = "[" ++ show k ++ "]"

-- | Given a 2-dimensional table of values, reports some statistics.
-- TODO: Revamp to use my spreadsheet class.
summarise :: [[Statistic]] -> [[Statistic]]
summarise xss = [maxima,minima,averages,stdDevs,sums]
  where yss = groupByName $ concat xss
        maxima   = compile "max. " maximum yss
        minima   = compile "min. " minimum yss
        averages = compile "avg. " mean yss
        stdDevs  = compile "std. dev. " popStdDev yss
        sums     = compile "total " sum yss

groupByName :: [Statistic] -> [(String, [Double])]
groupByName = map f . groupBy g . sortOn name
  where f xs = (name $ head xs, map value xs)
        g a b = name a == name b

compile :: String -> ([Double] -> Double) -> [(String, [Double])] -> [Statistic]
compile s f yss = map (prefix s) $ applyToColumns f yss

applyToColumns :: ([Double] -> Double) -> [(String, [Double])] -> [Statistic]
applyToColumns f = map (applyToColumn f)

applyToColumn :: ([Double] -> Double) -> (String, [Double]) -> Statistic
applyToColumn _ (_, []) = error "no data"
applyToColumn f (s, vs) = dStat s (f vs)

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
