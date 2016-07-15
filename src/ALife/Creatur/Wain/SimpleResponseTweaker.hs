------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SimpleResponseTweaker
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck test utilities.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.SimpleResponseTweaker
  (
    ResponseTweaker(..),
    responseDiff,
    makeResponseSimilar
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Difference, Tweaker(..))
import ALife.Creatur.Wain.PlusMinusOne (adjustPM1Vector)
import ALife.Creatur.Wain.Response (Response(..), labelSimilarity)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- | @'ResponseTweaker'@ constructs an object which is
--   responsible for comparing and adjusting response patterns.
--
--   NOTE: In order to declare an instance of @Tweaker@, we have to
--   reference the action type @a@. As a result, @ResponseTweaker@ has
--   to have a type parameter @a@, even though it is not used.
data ResponseTweaker a = ResponseTweaker
  deriving (Eq, Show, Generic, NFData, Serialize, W8.Genetic, Diploid)

instance (Eq a) => Tweaker (ResponseTweaker a) where
  type Pattern (ResponseTweaker a) = Response a
  diff _ = responseDiff
  adjust _ = makeResponseSimilar

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
