------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ResponseInternal
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private Response internals.
-- Most developers should use Response instead.
-- This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.ResponseInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Difference)
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.Scenario (Scenario,
  scenarioDiff, makeScenariosSimilarIgnoringLabels)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double,
  pm1ToDouble, adjustPM1Double, pm1Diff)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI,
  uiApply)
import ALife.Creatur.Wain.Weights (Weights, weightedSum)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Text.Printf (printf)

-- | A model of a situation that a wain might encounter.
data Response a = Response
  {
    -- | The situation to be responded to
    _scenario :: Scenario,
    -- | Action
    _action :: a,
    -- | Happiness level change (predicted or actual).
    _outcome :: PM1Double
  } deriving ( Eq, Show, Read, Generic, Ord, Serialize, Diploid,
               NFData )
makeLenses ''Response

instance (Genetic a) => Genetic (Response a)

instance (Show a) => Pretty (Response a) where
  pretty (Response s a o)
    = pretty s ++ '|':show a ++ '|':format o
    where format x =  printf "%.3f" $ pm1ToDouble x

-- | @'responseDiff' cw sw rw x y@ compares the response patterns
--   @x@ and @y@, and returns a number between 0 and 1, representing
--   how different the patterns are. A result of 0 indicates that the
--   patterns are identical.
--   The parameter @cw@ determines the relative weight to assign to
--   differences in energy, passion, and whether or not there is a
--   litter in each pattern.
--   The parameter @sw@ determines the relative weight to assign to
--   differences between each corresponding pair of objects represented
--   by the scenario being responded to in each pattern.
--   The parameter @rw@ determines the relative weight to assign to
--   differences in the scenarios and the outcomes in the two patterns.
responseDiff
  :: Eq a
    => Weights -> Weights -> Response a -> Response a -> Difference
responseDiff cw rw x y =
  if _action x == _action y
    then weightedSum rw ds
    else doubleToUI 1.0
  where ds = [sDiff, oDiff]
        sDiff = scenarioDiff cw (_scenario x) (_scenario y)
        oDiff = pm1Diff (_outcome x) (_outcome $ y)

diffIgnoringOutcome
  :: Eq a
    => Weights -> Weights -> Response a -> Response a -> Difference
diffIgnoringOutcome cw rw x y = responseDiff cw rw x' y'
  where x' = set outcome 0 x
        y' = set outcome 0 y

makeResponseSimilar
  :: Eq a
    => Response a -> UIDouble -> Response a -> Response a
makeResponseSimilar target r x =
    if _action target == _action x
       then Response s a o
       else x
    where s = makeScenariosSimilarIgnoringLabels (_scenario target) r
                (_scenario x)
          a = _action x -- never change this
          o = adjustPM1Double (_outcome target) r (_outcome x)

similarityIgnoringOutcome
  :: Eq a
    => Weights -> Weights -> Response a -> Response a -> UIDouble
similarityIgnoringOutcome cw rw x y
  = uiApply (\z -> 1 - z) $ diffIgnoringOutcome cw rw x y

-- | Updates the outcome in the second response to match the first.
copyOutcomeTo :: Response a -> Response a -> Response a
copyOutcomeTo source = set outcome (_outcome source)

-- responseSet
--   :: (Bounded a, Enum a) =>
--     Scenario -> PM1Double -> [Response a]
-- responseSet s o = map (\a -> Response s a o) as
--   where as = [minBound .. maxBound]
