------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Scenario
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A situation that a wain might encounter.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Scenario
  (
    Condition,
    Scenario(..),
    labels,
    condition,
    scenarioDiff,
    makeScenariosSimilarIgnoringLabels
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Difference, Label)
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiDoublesTo8BitHex,
  adjustUIVectorPreserveLength, doubleToUI, uiToDouble)
import ALife.Creatur.Wain.Weights (Weights, weightedUIVectorDiff)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.List (intercalate)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- | A wain's condition
type Condition = [UIDouble]

-- | A wain's assessment of a situation.
data Scenario = Scenario
  {
    -- | The classifier labels for each object in the scenario.
    _labels :: [Label],
    -- | Current condition
    _condition :: Condition
  } deriving ( Eq, Show, Read, Generic, Ord, Serialize, Genetic,
               Diploid, NFData )
makeLenses ''Scenario

-- | @'scenarioDiff' cw x y@ compares the scenarios
--   @x@ and @y@, and returns a number between 0 and 1,
--   representing how different the scenarios are.
--   A result of 0 indicates that the scenarios are identical,
--   apart from the labels.
--   The parameter @cw@ determines the relative weight to assign to
--   differences in each pair of condition components (e.g., energy,
--   passion, whether or not the wain has a litter).
scenarioDiff :: Weights -> Scenario -> Scenario -> Difference
scenarioDiff cw x y =
  if _labels x == _labels y
    then doubleToUI $ uiToDouble lDiff + uiToDouble cDiff / 2
    else 1
  where lDiff = 1 - labelSimilarity (_labels x) (_labels y)
        cDiff = weightedUIVectorDiff cw (_condition x) (_condition y)

labelSimilarity :: [Label] -> [Label] -> UIDouble
labelSimilarity xs ys =
  if count == 0
    then 1
    else doubleToUI $ fromIntegral matchCount / fromIntegral count
  where comparisons =  labelSimilarity' xs ys
        count = length comparisons
        matchCount = length $ filter id comparisons

labelSimilarity' :: [Label] -> [Label] -> [Bool]
labelSimilarity' (x:xs) (y:ys) = (x == y) : (labelSimilarity' xs ys)
labelSimilarity' (_:xs) [] = False : (labelSimilarity' xs [])
labelSimilarity' [] (_:ys) = False : (labelSimilarity' [] ys)
labelSimilarity' [] [] = []

-- | @'makeScenariosSimilar' target amount x@ returns a modified copy
--   of @x@ that is more similar to @target@ than @x@ is.
--   The magnitude of the adjustment is controlled by the @r@
--   parameter, which should be a number between 0 and 1. Larger
--   values for @r@ permit greater adjustments. If @r@=1,
--   the result should be identical to the @target@. If @r@=0,
--   the result should be the unmodified @pattern@.
makeScenariosSimilarIgnoringLabels
  :: Scenario -> UIDouble -> Scenario -> Scenario
makeScenariosSimilarIgnoringLabels target r x
  = Scenario (_labels x) $
      adjustUIVectorPreserveLength (_condition target) r (_condition x)

instance Pretty Scenario where
  pretty (Scenario ls c)
    = intercalate "|" (map show ls) ++ '|':uiDoublesTo8BitHex c
