------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Statistics
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
module ALife.Creatur.Wain.Statistics
  (
    Statistical,
    Statistic,
    dStat,
    iStat,
    stats,
    maxStats,
    minStats,
    avgStats,
    prefix
  ) where

import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import Data.Datamining.Clustering.SOM (DecayingGaussian(..))
import Data.List (foldl1')

data Statistic = DStatistic {sName :: String, sVal :: Double}
  | IStatistic {sName :: String, sVal :: Double}
  deriving (Eq)

instance Show Statistic where
  show (DStatistic s x) = "dStat \"" ++ s ++ "\" " ++ show x
  show (IStatistic s x) = "iStat \"" ++ s ++ "\" " ++ show (round x :: Int)

instance Pretty Statistic where
  pretty (DStatistic s x) = s ++ "=" ++ pretty x
  pretty (IStatistic s x) = s ++ "=" ++ pretty (round x :: Int)

instance Pretty [Statistic] where
  pretty [] = ""
  pretty [x] = pretty x
  pretty (x:xs) = pretty x ++ ',' : pretty xs

dStat :: Real a => String -> a -> Statistic
dStat s = DStatistic s . realToFrac

iStat :: String -> Int -> Statistic
iStat s v = IStatistic s (fromIntegral v)

class Statistical a where
  stats :: a -> [Statistic]

maxStats :: [[Statistic]] -> [Statistic]
maxStats = map (prefix "max ") . foldl1' (apply3 max)

minStats :: [[Statistic]] -> [Statistic]
minStats = map (prefix "min ") . foldl1' (apply3 min)

avgStats :: [[Statistic]] -> [Statistic]
avgStats xs
  = (map (prefix "avg " . apply (/ n)) . foldl1' (apply3 (+))) xs
    where n = fromIntegral $ length xs

apply3 :: (Double -> Double -> Double) -> [Statistic] -> [Statistic] -> [Statistic]
apply3 f = zipWith (apply2 f)

prefix :: String -> Statistic -> Statistic
prefix s x = x { sName = s ++ sName x }

apply :: (Double -> Double) -> Statistic -> Statistic
apply f x = x { sVal = f (sVal x) }

apply2 :: (Double -> Double -> Double) -> Statistic -> Statistic -> Statistic
apply2 f x y =
  if sName x == sName y
     then x { sVal = f (sVal x) (sVal y) }
     else DStatistic "???" 0

instance Statistical (DecayingGaussian Double) where
  stats (DecayingGaussian r0 rf w0 wf tf) = 
    [ dStat "r0" r0, dStat "rf" rf, iStat "w0" $ round w0,
      iStat "wf" $ round wf, iStat "tf" $ round tf ]
