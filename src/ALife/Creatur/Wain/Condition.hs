------------------------------------------------------------------------ 
-- |
-- Module      :  ALife.Creatur.Wain.Condition
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
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
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.Util (unitInterval, enforceRange,
  scaleFromWord8, scaleToWord8)
import ALife.Creatur.Wain.Random (RandomInitial(..))
import ALife.Creatur.Wain.Statistics (Statistical, stats, dStat)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Random (getRandom, getRandomR)
import Data.Datamining.Pattern (Pattern, Metric, adjustNum,
  difference, makeSimilar)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- TODO: Make these weights genetic
energyWeight :: UIDouble
energyWeight = 1 - passionWeight - boredomWeight

passionWeight :: UIDouble
passionWeight = 0.1

boredomWeight :: UIDouble
boredomWeight = 0.3

-- | A model of a stimulus and the response to it
data Condition = Condition
  {
    -- | Current energy level
    cEnergy :: UIDouble,
    -- | Current passion level
    cPassion :: UIDouble,
    -- | Current boredom level
    cBoredom :: UIDouble
  } deriving (Eq, Show, Generic)

instance Serialize Condition

-- An agent's current condition is not genetic, of course. However,
-- the initial response patterns stored at birth are genetically
-- determined, and they contain condition information.
instance Genetic Condition where
  put (Condition e p b) = do
    G.put . scaleToWord8 unitInterval $ e
    G.put . scaleToWord8 unitInterval $ p
    G.put . scaleToWord8 unitInterval $ b
  get = do
    e <- fmap (fmap . scaleFromWord8 $ unitInterval) G.get
    p <- fmap (fmap . scaleFromWord8 $ unitInterval) G.get
    b <- fmap (fmap . scaleFromWord8 $ unitInterval) G.get
    return $ Condition <$> e <*> p <*> b

instance Diploid Condition

instance Pattern Condition where
  type Metric Condition = UIDouble
  difference x y
    = sum [eDiff*energyWeight, pDiff*passionWeight,
            bDiff*boredomWeight]
    where eDiff = abs (cEnergy x - cEnergy y)
          pDiff = abs (cPassion x - cPassion y)
          bDiff = abs (cBoredom x - cBoredom y)
  makeSimilar target r x = Condition e p b
    where e = adjustNum (cEnergy target) r (cEnergy x)
          p = adjustNum (cPassion target) r (cPassion x)
          b = adjustNum (cBoredom target) r (cBoredom x)

instance Statistical Condition where
  stats (Condition e p b) =
    [dStat "energy" e, dStat "passion" p, dStat "boredom" b]

-- When wains are created, they have a predetermined initial condition.
-- However, we still need to generate random conditions for use in
-- response models.
instance RandomInitial Condition where
  randomInitial = do
    e <- getRandom
    p <- getRandomR (0, 1-e)
    b <- getRandomR (0, 1-e-p)
    return $ Condition e p b

initialCondition :: Condition
initialCondition = Condition 1 0 0

happiness :: Condition -> UIDouble
happiness (Condition e b p)
  = e*energyWeight + (1 - p)*passionWeight
      + (1 - b)*boredomWeight

alive :: Condition -> Bool
alive c = cEnergy c > 0

adjustEnergy :: UIDouble -> Condition -> Condition
adjustEnergy delta c = c {cEnergy=e}
  where e = enforceRange unitInterval (cEnergy c + delta)

adjustPassion :: UIDouble -> Condition -> Condition
adjustPassion delta c = c {cPassion=p}
  where p = enforceRange unitInterval (cPassion c + delta)

coolPassion :: Condition -> Condition
coolPassion c = c {cPassion=0}
