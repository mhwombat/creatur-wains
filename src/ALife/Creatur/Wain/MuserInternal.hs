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
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, pm1ToDouble)
import ALife.Creatur.Wain.Response (Response(..))
import ALife.Creatur.Wain.Scenario (Scenario(..))
import ALife.Creatur.Wain.Statistician (Probability)
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
    --   will use this value as a prediction.
    --   A positive value makes the wain optimistic and more likely to
    --   take risks. A negative value makes the wain pessimistic and
    --   risk-averse.
    _defaultOutcome :: PM1Double,
    -- | Number of possible scenarios a wain will evaluate before
    --   choosing an action.
    _depth :: Word8
  } deriving ( Eq, Read, Generic, Ord, Serialize, Genetic,
               Diploid, NFData )
makeLenses ''Muser

instance Show Muser where
  show (Muser o d) = "makeMuser (" ++ show o ++ ") " ++ show d

muserOK :: Muser -> Bool
muserOK m = _depth m > 0

instance Statistical Muser where
  stats (Muser o d) = [ dStat "def. outcome" (pm1ToDouble o),
                          iStat "depth" d ]

makeMuser :: PM1Double -> Word8 -> Muser
makeMuser o d =
  if d == 0
    then error "zero depth"
    else Muser o d

-- | Given the wain's current condition, and a set of hypotheses paired
--   with the probability that each hypothesis is true, returns a list
--   of responses to consider paired with the probability that the
--   response is based on an accurate scenario.
generateResponses
  :: (Bounded a, Enum a)
    => Muser -> [(Scenario, Probability)] -> [(Response a, Probability)]
generateResponses m sps = concatMap (generateResponses' m sps') as
  where sps' = bestHypotheses m sps
        as = [minBound .. maxBound]

generateResponses'
  :: (Bounded a, Enum a)
    => Muser -> [(Scenario, Probability)] -> a
      -> [(Response a, Probability)]
generateResponses' m sps a = map (generateResponse m a) sps

generateResponse
  :: (Bounded a, Enum a)
    => Muser -> a -> (Scenario, Probability)
      -> (Response a, Probability)
generateResponse m a (s, p) = (Response s a o, p)
  where o = _defaultOutcome m

-- | Given the wain's current condition, and a list of scenarios
--   paired with the probability each scenario is true, selects the
--   most likely scenarios.
bestHypotheses
  :: Muser -> [(Scenario, Probability)] -> [(Scenario, Probability)]
bestHypotheses m
  = take (fromIntegral . _depth $ m) . reverse. sortBy (comparing snd)
