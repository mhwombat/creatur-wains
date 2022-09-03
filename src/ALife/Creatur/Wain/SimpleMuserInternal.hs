------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SimpleMuserInternal
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private SimpleMuser internals.
-- Most developers should use SimpleMuser instead.
-- This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module ALife.Creatur.Wain.SimpleMuserInternal where

import ALife.Creatur.Gene.Numeric.PlusMinusOne qualified as PM1
import ALife.Creatur.Gene.Numeric.UnitInterval qualified as UI
import ALife.Creatur.Genetics.BRGCWord8        qualified as G
import ALife.Creatur.Genetics.Diploid          (Diploid)
import ALife.Creatur.Wain.GeneticSOM           (Label)
import ALife.Creatur.Wain.Muser                qualified as M
import ALife.Creatur.Wain.Pretty               (Pretty)
import ALife.Creatur.Wain.Response             (Response (..))
import ALife.Creatur.Wain.Statistics           (Statistical, dStat, iStat,
                                                stats)
import Control.DeepSeq                         (NFData)
import Data.List                               (sortOn)
import Data.Ord                                (Down (..))
import Data.Serialize                          (Serialize)
import Data.Word                               (Word8)
import GHC.Generics                            (Generic)


-- | Object responsible for generating potential responses for
--   consideration.
data SimpleMuser a = SimpleMuser
  {
    -- | If a wain has no model for a response it's considering, it
    --   will use these values as a prediction.
    --   Positive values make the wain optimistic and more likely to
    --   take risks. A negative value makes the wain pessimistic and
    --   risk-averse.
    defaultOutcomes :: [PM1.Double],
    -- | Number of possible scenarios a wain will evaluate before
    --   choosing an action.
    depth           :: Word8
  } deriving ( Eq, Show, Read, Generic, Ord, Serialize, Diploid, NFData )

instance Pretty (SimpleMuser a)

instance Statistical (SimpleMuser a) where
  stats (SimpleMuser (eo:po:lso:_) d) = [iStat "depth" d,
         dStat "default energy outcome" . PM1.wide $ eo,
         dStat "default passion outcome" . PM1.wide $ po,
         dStat "default litterSize outcome" . PM1.wide $ lso]
  stats _ = error "default outcome list is too short"


instance G.Genetic (SimpleMuser a) where
  put (SimpleMuser o d) = G.put o >> G.put d
  get = do
    o <- G.get
    d <- G.get
    -- Use the safe constructor!
    case makeMuser <$> o <*> d of
      Left msgs -> return $ Left msgs
      Right b   -> return b

instance (Bounded a, Enum a, Eq a) => M.Muser (SimpleMuser a) where
  type Action (SimpleMuser a) = a
  generateResponses = generateResponses
  defaultOutcomes = defaultOutcomes

-- | Constructor
makeMuser :: [PM1.Double] -> Word8 -> Either [String] (SimpleMuser a)
makeMuser os d
 | d == 0         = Left ["zero depth"]
 | length os < 3 = Left ["default outcome list is too short"]
 | otherwise     = Right $ SimpleMuser os d

generateResponses
  :: (Bounded a, Enum a, Eq a)
    => SimpleMuser a -> [a] -> [([Label], UI.Double)] -> [(Response a, UI.Double)]
generateResponses m _ sps = concatMap (generateResponses' m sps') as'
  where sps' = bestHypotheses m sps
        as' = [minBound .. maxBound]

-- | Internal method
generateResponses'
  :: (Bounded a, Enum a)
    => SimpleMuser a -> [([Label], UI.Double)] -> a
      -> [(Response a, UI.Double)]
generateResponses' m sps a = map (generateResponse m a) sps

-- | Internal method
generateResponse
  :: (Bounded a, Enum a)
    => SimpleMuser a -> a -> ([Label], UI.Double)
      -> (Response a, UI.Double)
generateResponse m a (ls, p) = (Response ls a os, p)
  where os = defaultOutcomes m

-- | Given the wain's current condition, and a list of scenarios
--   paired with the probability each scenario is true, selects the
--   most likely scenarios.
bestHypotheses
  :: SimpleMuser a -> [([Label], UI.Double)] -> [([Label], UI.Double)]
bestHypotheses m
  = take (fromIntegral . depth $ m) . sortOn (Down . snd)
