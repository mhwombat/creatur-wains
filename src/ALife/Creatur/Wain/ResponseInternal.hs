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
import ALife.Creatur.Wain.GeneticSOM (Difference, Label)
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, pm1ToDouble,
  adjustPM1Vector, forceDoubleToPM1)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.List (intercalate)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Text.Printf (printf)

-- | A model of a scenario that a wain might encounter
data Response a = Response
  {
    -- | The classifier labels for the objects we're responding to
    _labels :: [Label],
    -- | Action
    _action :: a,
    -- | Happiness level change (predicted or actual)
    _outcomes :: [PM1Double]
  } deriving ( Eq, Show, Read, Generic, Ord, Serialize, Diploid,
               NFData )
makeLenses ''Response

instance (Genetic a) => Genetic (Response a)

instance (Show a) => Pretty (Response a) where
  pretty (Response ls a os) =
    intercalate "|" (map show ls) ++ '|':show a ++ '|':format os
    where format xs =  intercalate "|" . map (printf "%.3f" .  pm1ToDouble) $ xs

-- | @'responseDiff' x y@ compares the response patterns
--   @x@ and @y@, and returns a number between 0 and 1, representing
--   how different the patterns are. A result of 0 indicates that the
--   patterns are identical, apart from the outcome.
responseDiff
  :: Eq a
    => Response a -> Response a -> Difference
responseDiff x y =
  if _action x == _action y
    then 1 - labelSimilarity (_labels x) (_labels y)
    else 1

-- | Internal method
labelSimilarity :: [Label] -> [Label] -> UIDouble
labelSimilarity xs ys =
  if count == 0
    then 1
    else doubleToUI $ fromIntegral matchCount / fromIntegral count
  where comparisons =  labelSimilarity' xs ys
        count = length comparisons
        matchCount = length $ filter id comparisons

-- | Internal method
labelSimilarity' :: [Label] -> [Label] -> [Bool]
labelSimilarity' (x:xs) (y:ys) = (x == y) : (labelSimilarity' xs ys)
labelSimilarity' (_:xs) [] = False : (labelSimilarity' xs [])
labelSimilarity' [] (_:ys) = False : (labelSimilarity' [] ys)
labelSimilarity' [] [] = []

-- | @'makeResponseSimilar' target r pattern@ returns a modified copy
--   of @pattern@ that is more similar to @target@ than @pattern@ is.
--   The magnitude of the adjustment is controlled by the @r@
--   parameter, which should be a number between 0 and 1. Larger
--   values for @r@ permit greater adjustments. If @r@=1,
--   the result should be identical to the @target@. If @r@=0,
--   the result should be the unmodified @pattern@.
makeResponseSimilar
  :: Eq a
    => Response a -> UIDouble -> Response a -> Response a
makeResponseSimilar target r x =
    if _action target == _action x
       then Response s a o
       else x
    where s = _labels x -- never change this
          a = _action x -- never change this
          o = adjustPM1Vector (_outcomes target) r (_outcomes x)

-- | Updates the outcomes in the second response to match the first.
copyOutcomesTo :: Response a -> Response a -> Response a
copyOutcomesTo source = set outcomes (_outcomes source)

-- | Increment the outcomes in a response by the specified amount.
--   Note: Outcomes are capped at 1.
addToOutcomes :: [PM1Double] -> Response a -> Response a
addToOutcomes deltas r = set outcomes ys r
  where xs = map pm1ToDouble . _outcomes $ r
        deltas' = map pm1ToDouble deltas
        ys = map forceDoubleToPM1 $ zipWith (+) xs deltas'
