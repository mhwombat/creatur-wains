------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Muser
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- The muser takes classification information (labels and
-- probabilities) about the objects in a wain's field of "view",
-- and determines the scenarios that the wain is likely facing.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ALife.Creatur.Wain.Muser
  (
    Muser,
    makeMuser,
    muserOK,
    _defaultOutcome,
    defaultOutcome,
    depth,
    _depth,
    generateResponses,
    mostLikelyScenarios
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Classifier (Label, Signature)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, pm1ToDouble)
import ALife.Creatur.Wain.Response (Response(..), responseSet)
import ALife.Creatur.Wain.Scenario (Scenario(..))
import ALife.Creatur.Wain.Statistics (Statistical, stats, iStat, dStat)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
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
  } deriving ( Eq, Show, Read, Generic, Ord, Serialize, Genetic,
               Diploid, NFData )
makeLenses ''Muser

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

-- | Given the wain's current condition, and the output from the
--   classifier, returns a list of responses to consider.
generateResponses
  :: (Bounded a, Enum a)
    => Muser -> [Signature] -> [UIDouble]
      -> [(Response a, UIDouble)]
generateResponses p lds c = concatMap (generateResponses' p) sps
  where sps = mostLikelyScenarios p c lds

generateResponses'
  :: (Bounded a, Enum a)
    => Muser -> (Scenario, UIDouble) -> [(Response a, UIDouble)]
generateResponses' p (s, k) = zip (responseSet s o) (repeat k)
  where o = _defaultOutcome p

-- | Given the wain's current condition, and the output from the
--   classifier, returns a list of the most likely scenarios,
--   together with the likelihood of that scenario.
mostLikelyScenarios
  :: Muser -> [UIDouble] -> [Signature]
    -> [(Scenario, UIDouble)]
mostLikelyScenarios p c
  = take (fromIntegral . _depth $ p) . reverse . sortBy (comparing snd)
      . generateScenarios c

-- | Given the wain's current condition, and the output from the
--   classifier, returns a list of all possible scenarios,
--   together with the likelihood of that scenario.
generateScenarios
  :: [UIDouble] -> [Signature] -> [(Scenario, UIDouble)]
generateScenarios c
  = map (\(ls, p) -> (Scenario ls c, p)) . map likelihood . permute

permute :: [[a]] -> [[a]]
permute (xs:[]) = [ [y] | y <- xs ]
permute (xs:xss) = [ y:ys | y <- xs, ys <- permute xss]
permute [] = []

-- | Given a list of labels for each object the wain is seeing, paired
--   with the probability that each label is accurate, returns
--   the probability that all labels are accurate.
likelihood :: [(Label, UIDouble)] -> ([Label], UIDouble)
likelihood xs = (ls, 1 - prob)
  where ls = map fst xs
        prob = product . map snd $ xs
