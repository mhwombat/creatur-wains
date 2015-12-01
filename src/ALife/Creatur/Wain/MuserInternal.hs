------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.MuserInternal
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private Muser internals.
-- Most developers should use Muser instead.
-- This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ALife.Creatur.Wain.MuserInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Label)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, pm1ToDouble)
import ALife.Creatur.Wain.Response (Response(..))
import ALife.Creatur.Wain.Probability (Probability)
import ALife.Creatur.Wain.Statistics (Statistical, stats, iStat, dStat)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)

data Muser = Muser
  {
    -- | If a wain has no model for a response it's considering, it
    --   will use these values as a prediction.
    --   Positive values make the wain optimistic and more likely to
    --   take risks. A negative value makes the wain pessimistic and
    --   risk-averse.
    _defaultOutcomes :: [PM1Double],
    -- | Number of possible scenarios a wain will evaluate before
    --   choosing an action.
    _depth :: Word8
  } deriving ( Eq, Read, Generic, Ord, Serialize, Genetic,
               Diploid, NFData )
makeLenses ''Muser

instance Show Muser where
  show (Muser o d) = "makeMuser (" ++ show o ++ ") " ++ show d

instance Statistical Muser where
  stats (Muser os d) = [iStat "depth" d,
         dStat "default energy outcome" . pm1ToDouble $ os !! 0,
         dStat "default passion outcome" . pm1ToDouble $ os !! 1,
         dStat "default boredom outcome" . pm1ToDouble $ os !! 2,
         dStat "default litterSize outcome" . pm1ToDouble $ os !! 3]

makeMuser :: [PM1Double] -> Word8 -> Muser
makeMuser os d =
  if d == 0
    then error "zero depth"
    else Muser os d

-- | Given a set of scenarios paired with the probability that each
--   scenario is true, returns a list of responses to consider paired
--   with the probability that the response is based on the correct
--   scenario.
generateResponses
  :: (Bounded a, Enum a)
    => Muser -> [([Label], Probability)] -> [(Response a, Probability)]
generateResponses m sps = concatMap (generateResponses' m sps') as
  where sps' = bestHypotheses m sps
        as = [minBound .. maxBound]

generateResponses'
  :: (Bounded a, Enum a)
    => Muser -> [([Label], Probability)] -> a
      -> [(Response a, Probability)]
generateResponses' m sps a = map (generateResponse m a) sps

generateResponse
  :: (Bounded a, Enum a)
    => Muser -> a -> ([Label], Probability)
      -> (Response a, Probability)
generateResponse m a (ls, p) = (Response ls a os, p)
  where os = _defaultOutcomes m

-- | Given the wain's current condition, and a list of scenarios
--   paired with the probability each scenario is true, selects the
--   most likely scenarios.
bestHypotheses
  :: Muser -> [([Label], Probability)] -> [([Label], Probability)]
bestHypotheses m
  = take (fromIntegral . _depth $ m) . reverse. sortBy (comparing snd)