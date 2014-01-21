------------------------------------------------------------------------ 
-- |
-- Module      :  ALife.Creatur.Wain.Condition
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tracks an agent's state of health and happiness.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, TypeFamilies #-}
module ALife.Creatur.Wain.Condition
  (
    Condition(..),
    initialCondition,
    alive,
    happiness,
    adjustEnergy,
    adjustPassion,
    coolPassion
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Wain.Util (unitInterval, enforceRange,
  scaleFromWord8, scaleToWord8)
import ALife.Creatur.Wain.Random (RandomInitial(..))
import ALife.Creatur.Wain.Statistics (Statistical, stats, dStat)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Random (getRandom)
import Data.Datamining.Pattern (Pattern, Metric, adjustNum,
  difference, makeSimilar)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- TODO: Make these weights genetic
energyWeight :: Double
energyWeight = 1 - passionWeight

passionWeight :: Double
passionWeight = 0.1

-- | A model of a stimulus and the response to it
data Condition = Condition
  {
    -- | Current energy level
    cEnergy :: Double,
    -- | Current passion level
    cPassion :: Double
  } deriving (Eq, Show, Generic)

instance Serialize Condition

-- An agent's current condition is not genetic, of course. However,
-- the initial response patterns stored at birth are genetically
-- determined, and they contain condition information.
instance Genetic Condition where
  put (Condition e p) = do
    G.put $ scaleToWord8 unitInterval e
    G.put $ scaleToWord8 unitInterval p
  get = do
    e <- fmap (fmap $ scaleFromWord8 unitInterval) G.get
    p <- fmap (fmap $ scaleFromWord8 unitInterval) G.get
    return $ Condition <$> e <*> p

instance Diploid Condition

instance Pattern Condition where
  type Metric Condition = Double
  difference x y
    = sum [eDiff*energyWeight, pDiff*passionWeight]
    where eDiff = abs (cEnergy x - cEnergy y)
          pDiff = abs (cPassion x - cPassion y)
  makeSimilar target r x = Condition e p
    where e = adjustNum (cEnergy target) r (cEnergy x)
          p = adjustNum (cPassion target) r (cPassion x)

instance Statistical Condition where
  stats c@(Condition e p) =
    [dStat "energy" e, dStat "passion" p,
     dStat "happiness" (happiness c)]

-- When wains are created, they have a predetermined initial condition.
-- However, we still need to generate random conditions for use in
-- response models.
instance RandomInitial Condition where
  randomInitial = do
    e <- getRandom
    p <- getRandom
    return $ Condition e p

initialCondition :: Condition
initialCondition = Condition 1 0

happiness :: Condition -> Double
happiness (Condition e p)
  = e*energyWeight + (1 - p)*passionWeight

alive :: Condition -> Bool
alive c = cEnergy c > 0

adjustEnergy :: Double -> Condition -> Condition
adjustEnergy delta c = c {cEnergy=e}
  where e = enforceRange unitInterval (cEnergy c + delta)

adjustPassion :: Double -> Condition -> Condition
adjustPassion delta c = c {cPassion=p}
  where p = enforceRange unitInterval (cPassion c + delta)

coolPassion :: Condition -> Condition
coolPassion c = c {cPassion=0}
