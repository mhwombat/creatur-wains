------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Response
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
module ALife.Creatur.Wain.Response
  (
    Response(..),
    copyOutcomeTo,
    setOutcome,
    possibleResponses,
    randomResponse
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Scenario (Scenario, randomScenario)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Random (Rand, RandomGen, getRandom)
import Data.Datamining.Pattern (Pattern, Metric, difference,
  makeSimilar)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Random (Random)

-- Weights to use when comparing possible courses of action.
-- The values should add up to one.
-- TODO: Make genetic
deciderWeights :: [UIDouble]
deciderWeights =
  [
    0.3, -- scenario
    0.1  -- result
  ]

-- | A model of a situation and the response to it
data Response a = Response
  {
    -- | The situation to be responded to
    scenario :: Scenario,
    -- | Action
    action :: a,
    -- | Happiness level change (predicted or actual).
    --   Response patterns stored in the decider will have a @Just@
    --   value; stimuli received from the outside will have a @Nothing@
    --   value.
    outcome :: Maybe UIDouble
  } deriving (Eq, Show, Generic)

instance (Serialize a) => Serialize (Response a)

instance (Eq a) => Pattern (Response a) where
  type Metric (Response a) = UIDouble
  difference x y =
    if action x == action y
      then 0.0
      else (sum . zipWith (*) deciderWeights $ ds)/2
    where ds = [sDiff, rDiff]
          sDiff = difference (scenario x) (scenario y)
          rDiff = case outcome x of
                    (Just rx) ->
                      case outcome y of
                        (Just ry) -> abs (rx - ry)
                        Nothing -> 1
                    Nothing -> 1
  makeSimilar target r x = Response s a o
    where s = makeSimilar (scenario target) r (scenario x)
          a = action x
          o = Just $ makeSimilar (fromMaybe 0.0 . outcome $ target) r (fromMaybe 0.0 . outcome $ x)

-- | The initial sequences stored at birth are genetically determined.
instance (Genetic a) => Genetic (Response a)

instance (Diploid a) => Diploid (Response a)

randomResponse
  :: (RandomGen g, Random a)
    => Int -> Rand g (Response a)
randomResponse n = Response <$> randomScenario n <*> getRandom <*> fmap Just getRandom

possibleResponses :: (Enum a, Bounded a) => Scenario -> [Response a]
possibleResponses s
  = map (\a -> Response s a Nothing) [minBound..maxBound]

copyOutcomeTo :: Response a -> Response a -> Response a
copyOutcomeTo source target = target { outcome=outcome source }

setOutcome :: Response a -> UIDouble -> Response a
setOutcome r o = r { outcome=Just o }

-- getOutcome :: Response s a -> Double
-- getOutcome = outcome
