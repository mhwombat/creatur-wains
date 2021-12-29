------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PatternAdjuster
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Learning functions for self-organising maps.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
module ALife.Creatur.Wain.PatternAdjuster
  (
    PatternAdjuster(..)
  ) where

import           ALife.Creatur.Gene.Numeric.UnitInterval (UIDouble)
import qualified ALife.Creatur.Genetics.BRGCWord8        as G
import           ALife.Creatur.Genetics.Diploid          (Diploid)
import           ALife.Creatur.Wain.LearningParams       (LearningParams,
                                                          toLearningFunction)
import qualified ALife.Creatur.Wain.Pattern              as P
import           ALife.Creatur.Wain.Report               (Report, report)
import           ALife.Creatur.Wain.Statistics           (Statistical, stats)
import           Control.DeepSeq                         (NFData)
import           Data.Datamining.Clustering.SGM4Internal (Adjuster)
import qualified Data.Datamining.Clustering.SGM4Internal as SOM
import           Data.Serialize                          (Serialize)
import           Data.Word                               (Word32)
import           GHC.Generics                            (Generic)

data PatternAdjuster p = PatternAdjuster LearningParams
  deriving (Eq, Show, Read, Generic, Serialize, G.Genetic, Diploid, NFData)

instance (P.Pattern p) => Adjuster (PatternAdjuster p) where
  type TimeType (PatternAdjuster p) = Word32
  type MetricType (PatternAdjuster p) = UIDouble
  type PatternType (PatternAdjuster p) = p
  learningRate (PatternAdjuster l) = toLearningFunction l
  difference _ = P.diff
  makeSimilar _ = P.makeSimilar

instance Statistical (PatternAdjuster p) where
  stats (PatternAdjuster l) = stats l

instance Report (PatternAdjuster p) where
  report (PatternAdjuster l) = report l
