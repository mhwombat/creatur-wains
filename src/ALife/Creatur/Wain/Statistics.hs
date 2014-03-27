------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Statistics
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Statistical calculations
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
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
    uiStat,
    summarise
  ) where

import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import Data.Datamining.Clustering.SOM (DecayingGaussian(..))
import Data.List (transpose, intercalate)
import Data.Serialize (Serialize)
import Factory.Math.Statistics (getMean, getStandardDeviation)
import GHC.Generics
import Text.Printf (printf)

data Statistic = DStatistic {sName :: String, sVal :: Double}
  | IStatistic {sName :: String, sVal :: Double}
  | UIStatistic {sName :: String, sVal :: Double}
  deriving (Eq, Generic)

instance Serialize Statistic

instance Show Statistic where
  show (DStatistic s x) = "dStat \"" ++ s ++ "\" " ++ show x
  show (UIStatistic s x) = "dStat \"" ++ s ++ "\" " ++ show x
  show (IStatistic s x) = "iStat \"" ++ s ++ "\" " ++ show (round x :: Int)

instance Pretty Statistic where
  pretty (DStatistic s x) = s ++ "=" ++ pretty x
  pretty (UIStatistic s x) = s ++ "=" ++  printf "%.3f" x
  pretty (IStatistic s x) = s ++ "=" ++ pretty (round x :: Int)

instance Pretty [Statistic] where
  pretty [] = ""
  pretty [x] = pretty x
  pretty (x:xs) = pretty x ++ ',' : pretty xs

dStat :: Real a => String -> a -> Statistic
dStat s = DStatistic s . realToFrac

uiStat :: Real a => String -> a -> Statistic
uiStat s = UIStatistic s . realToFrac

iStat :: Integral a => String -> a -> Statistic
iStat s v = IStatistic s (fromIntegral v)

prefix :: String -> Statistic -> Statistic
prefix s x = x { sName = s ++ sName x }

apply :: (Double -> Double) -> Statistic -> Statistic
apply f x = x { sVal = f (sVal x) }

name :: Statistic -> String
name = sName

class Statistical a where
  stats :: a -> [Statistic]

summarise :: [[Statistic]] -> [String]
summarise xss = [maxima,minima,averages,stdDevs,sums]
  where yss = transpose xss
        maxima   = compile "max. " maximum yss
        minima   = compile "min. " minimum yss
        averages = compile "avg. " getMean yss
        stdDevs  = compile "std. dev. " getStandardDeviation yss
        sums     = compile "total " sum yss
-- summarise xss = [h,maxima,minima,averages,stdDevs,sums]
--   where h = header xss
--         yss = transpose xss
--         maxima = "max," ++ (toCSV $ applyToColumns maximum yss)
--         minima = "min," ++ (toCSV $ applyToColumns minimum yss)
--         averages = "avg," ++ (toCSV $ applyToColumns getMean yss)
--         stdDevs = "std. dev,"
--                     ++ (toCSV $ applyToColumns getStandardDeviation yss)
--         sums = "total," ++ (toCSV $ applyToColumns sum yss)

compile :: String -> ([Double] -> Double) -> [[Statistic]] -> String
compile s f yss = intercalate "," . map (pretty . prefix s) $ applyToColumns f yss

-- header :: [[Statistic]] -> String
-- header [] = "no data"
-- header (a:_) = "headings," ++ (intercalate "," . map sName $ a)

-- toCSV :: [Statistic] -> String
-- toCSV = intercalate "," . map prettyVal

applyToColumns :: ([Double] -> Double) -> [[Statistic]] -> [Statistic]
applyToColumns f xss = map (applyToColumn f) xss

applyToColumn :: ([Double] -> Double) -> [Statistic] -> Statistic
applyToColumn f xs@(y:_) = y { sVal=f (map sVal xs) }
applyToColumn _ [] = error "no data"

-- prettyVal :: Statistic -> String
-- prettyVal (DStatistic _ x) = pretty x
-- prettyVal (IStatistic _ x) = pretty (round x :: Int)

instance Statistical (DecayingGaussian Double) where
  stats (DecayingGaussian r0 rf w0 wf tf) = 
    [ dStat "r0" r0, dStat "rf" rf, iStat "w0" (round w0 :: Int),
      iStat "wf" (round wf :: Int), iStat "tf" (round tf :: Int) ]
