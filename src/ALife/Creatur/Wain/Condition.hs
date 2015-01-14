------------------------------------------------------------------------ 
-- |
-- Module      :  ALife.Creatur.Wain.Condition
-- Copyright   :  (c) Amy de Buitléir 2013-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A wain's "condition" includes anything it needs to know about its
-- current status in order to make good decisions.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Condition
  (
    Condition(..),
    randomCondition,
    happiness
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.Util (unitInterval, scaleFromWord8,
  scaleToWord8, doubleTo8BitHex)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Random (Rand, RandomGen, getRandom)
import Data.Datamining.Pattern (Pattern, Metric, adjustNum,
  difference, makeSimilar)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Printf (printf)

-- Weights used for calculating the similarity between two conditions
energySimWeight :: Double
energySimWeight = 1 - passionSimWeight - litterSimWeight

passionSimWeight :: Double
passionSimWeight = 0.1

litterSimWeight :: Double
litterSimWeight = 0.1

-- Weights used for calculating happiness
-- TODO: Make these weights genetic
energyHapWeight :: Double
energyHapWeight = 1 - passionHapWeight

passionHapWeight :: Double
passionHapWeight = 0.3

-- | A model of a stimulus and the response to it
data Condition = Condition
  {
    -- | Current energy level
    cEnergy :: Double,
    -- | Current passion level
    cPassion :: Double,
    -- | Is the wain currently rearing a litter?
    cLitterSize :: Word8
  } deriving (Eq, Show, Generic)

instance Serialize Condition

-- An agent's current condition is not genetic, of course. However,
-- the initial response patterns stored at birth are genetically
-- determined, and they contain condition information.
instance Genetic Condition where
  put (Condition e p l) = do
    G.put $ scaleToWord8 unitInterval e
    G.put $ scaleToWord8 unitInterval p
    G.put l
  get = do
    e <- fmap (fmap $ scaleFromWord8 unitInterval) G.get
    p <- fmap (fmap $ scaleFromWord8 unitInterval) G.get
    l <- G.get
    return $ Condition <$> e <*> p <*> l

instance Diploid Condition

instance Pattern Condition where
  type Metric Condition = Double
  difference x y
    = sum [eDiff*energySimWeight, pDiff*passionSimWeight, lDiff*litterSimWeight]
    where eDiff = abs (cEnergy x - cEnergy y)
          pDiff = abs (cPassion x - cPassion y)
          lDiff = if cLitterSize x == cLitterSize y then 0 else 1
  makeSimilar target r x = Condition e p l
    where e = adjustNum (cEnergy target) r (cEnergy x)
          p = adjustNum (cPassion target) r (cPassion x)
          l = round $ adjustNum lTarget r lX
          lTarget = fromIntegral $ cLitterSize target :: Double
          lX = fromIntegral $ cLitterSize x :: Double

-- instance Statistical Condition where
--   stats c@(Condition e p l) =
--     [dStat "energy" e, dStat "passion" p,
--      dStat "happiness" (happiness c)]

-- | Returns a random condition.
--   This is useful for generating random response models.
randomCondition :: RandomGen g => Rand g Condition
randomCondition = do
    e <- getRandom
    p <- getRandom
    l <- getRandom
    return $ Condition e p l

-- | Wains seek to maximise their happiness, which is a function
--   their current condition.
--   TODO: Should the litter size affect happiness?
happiness :: Condition -> Double
happiness (Condition e p _)
  = e*energyHapWeight + (1 - p)*passionHapWeight

instance Pretty Condition where
  pretty (Condition e p l)
    = doubleTo8BitHex e ++ doubleTo8BitHex p ++ printf "%.2X" l
