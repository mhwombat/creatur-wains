------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SimpleMuser
-- Copyright   :  (c) Amy de Buitléir 2013-2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A muser that works with enumerable actions.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module ALife.Creatur.Wain.SimpleMuser where

import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (Label)
import qualified ALife.Creatur.Wain.Muser as M
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, pm1ToDouble)
import ALife.Creatur.Wain.Response (Response(..))
import ALife.Creatur.Wain.Probability (Probability)
import ALife.Creatur.Wain.Statistics (Statistical, stats, iStat, dStat)
import ALife.Creatur.Wain.Pretty (Pretty)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.List (sortBy, nub)
import Data.Ord (comparing)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)


-- | Object responsible for generating potential responses for
--   consideration.
data SimpleMuser a = SimpleMuser
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
  } deriving ( Eq, Read, Generic, Ord, Serialize, Diploid, NFData )
makeLenses ''SimpleMuser

instance Show (SimpleMuser a) where
  show (SimpleMuser o d) = "makeSimpleMuser " ++ show o ++ " " ++ show d

instance Pretty (SimpleMuser a)

instance Statistical (SimpleMuser a) where
  stats (SimpleMuser (eo:po:bo:lso:_) d) = [iStat "depth" d,
         dStat "default energy outcome" . pm1ToDouble $ eo,
         dStat "default passion outcome" . pm1ToDouble $ po,
         dStat "default boredom outcome" . pm1ToDouble $ bo,
         dStat "default litterSize outcome" . pm1ToDouble $ lso]
  stats _ = error "default outcome list is too short"


instance G.Genetic (SimpleMuser a) where
  put (SimpleMuser o d) = G.put o >> G.put d
  get = do
    o <- G.get
    d <- G.get
    -- Use the safe constructor!
    case (makeMuser <$> o <*> d) of
      Left msgs -> return $ Left msgs
      Right b   -> return b

instance (Bounded a, Enum a, Eq a) => M.Muser (SimpleMuser a) where
  type Action (SimpleMuser a) = a
  generateResponses = generateResponses
  defaultOutcomes = view defaultOutcomes

-- | Constructor
makeMuser :: [PM1Double] -> Word8 -> Either [String] (SimpleMuser a)
makeMuser os d
 | d == 0         = Left ["zero depth"]
 | length os < 4 = Left ["default outcome list is too short"]
 | otherwise     = Right $ SimpleMuser os d

generateResponses
  :: (Bounded a, Enum a, Eq a)
    => SimpleMuser a -> [a] -> [([Label], Probability)] -> [(Response a, Probability)]
generateResponses m as sps = concatMap (generateResponses' m sps') as'
  where sps' = bestHypotheses m sps
        as' = nub $ as ++ [minBound .. maxBound]

-- | Internal method
generateResponses'
  :: (Bounded a, Enum a)
    => SimpleMuser a -> [([Label], Probability)] -> a
      -> [(Response a, Probability)]
generateResponses' m sps a = map (generateResponse m a) sps

-- | Internal method
generateResponse
  :: (Bounded a, Enum a)
    => SimpleMuser a -> a -> ([Label], Probability)
      -> (Response a, Probability)
generateResponse m a (ls, p) = (Response ls a os, p)
  where os = _defaultOutcomes m

-- | Given the wain's current condition, and a list of scenarios
--   paired with the probability each scenario is true, selects the
--   most likely scenarios.
bestHypotheses
  :: SimpleMuser a -> [([Label], Probability)] -> [([Label], Probability)]
bestHypotheses m
  = take (fromIntegral . _depth $ m) . reverse. sortBy (comparing snd)
