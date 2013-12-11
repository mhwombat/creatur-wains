------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Scenario
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleContexts #-}
module ALife.Creatur.Wain.Scenario
  (
    Scenario(..),
    randomScenario
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Condition (Condition)
import ALife.Creatur.Wain.Random (randomInitial)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Monad (replicateM)
import Control.Monad.Random (Rand, RandomGen, getRandom)
import Data.Datamining.Pattern (Pattern, Metric, difference,
  makeSimilar)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- Weights to use when comparing possible courses of action.
-- The values should add up to one.
-- TODO: Make genetic
scenarioWeights :: [UIDouble]
scenarioWeights =
  [
    0.5, -- direct object signature
    0.3, -- indirect object signature
    0.2 -- condition
  ]

-- | A wain's assessment of a situation.
data Scenario = Scenario
  {
    -- | The pattern probabilities identified by the classifier
    --   for the direct object of the action.
    directObject :: [UIDouble],
    -- | The pattern probabilities identified by the classifier
    --   for the indirect object of the action.
    indirectObject :: [UIDouble],
    -- | Current condition
    condition :: Condition
  } deriving (Eq, Show, Generic)

instance Serialize Scenario

instance Pattern Scenario where
  type Metric Scenario = UIDouble
  difference x y = (sum . zipWith (*) scenarioWeights $ ds)/3
    where ds = [doDiff, ioDiff, cDiff]
          doDiff = difference (directObject x) (directObject y)
          ioDiff = difference (indirectObject x) (indirectObject y)
          cDiff = difference (condition x) (condition y)
  makeSimilar target r x = Scenario dObj iObj cond
    where dObj = makeSimilar (directObject target) r (directObject x)
          iObj = makeSimilar (indirectObject target) r (indirectObject x)
          cond = makeSimilar (condition target) r (condition x)

-- | The initial sequences stored at birth are genetically determined.
instance Genetic Scenario

instance Diploid Scenario

randomScenario :: RandomGen g => Int -> Rand g Scenario
randomScenario n = do
  ds <- replicateM n getRandom
  is <- replicateM n getRandom
  c <- randomInitial
  return $ Scenario ds is c
