------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ResponseInternal
-- Copyright   :  (c) Amy de Buitléir 2013-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private Response internals.
-- Most developers should use Response instead.
-- This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module ALife.Creatur.Wain.ResponseInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic, Reader, put, get)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.Scenario (Scenario, randomScenario)
import ALife.Creatur.Wain.Util (scaleFromWord8, scaleToWord8)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Random (Rand, RandomGen, getRandom, getRandomR)
import Data.Datamining.Pattern (Pattern, Metric, difference,
  makeSimilar)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)
import System.Random (Random)
import Text.Printf (printf)

outcomeInterval :: (Double, Double)
outcomeInterval = (-1.0,1.0)

-- Weights to use when comparing possible courses of action.
-- The values should add up to one.
-- TODO: Make genetic
deciderWeights :: [Double]
deciderWeights =
  [
    0.3, -- scenario
    0.1  -- result
  ]

-- | A model of a situation that a wain might encounter.
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
    outcome :: Maybe Double
  } deriving (Eq, Show, Generic)
  
instance (Serialize a) => Serialize (Response a)

instance (Eq a) => Pattern (Response a) where
  type Metric (Response a) = Double
  difference x y =
    if action x == action y
      then sum (zipWith (*) deciderWeights ds) / 2
      else 1.0
    where ds = [sDiff, rDiff]
          sDiff = difference (scenario x) (scenario y)
          rDiff = diffOutcome (fromMaybe 0 $ outcome x)
                    (fromMaybe 0 $ outcome x)
  makeSimilar target r x =
    if action target == action x
       then Response s a o
       else x
    where s = makeSimilar (scenario target) r (scenario x)
          a = action x
          o = Just $ makeSimilar (fromMaybe 0.0 $ outcome target) r
                (fromMaybe 0.0 $ outcome x)

diffOutcome :: Double -> Double -> Double
diffOutcome a b = abs (a-b) / 2
  -- a and b are in the interval [-1,1]. We want the difference to be
  -- in the interval [0,1]

similarityIgnoringOutcome :: Eq a => Response a -> Response a -> Double
similarityIgnoringOutcome x y = 1 - difference x' y'
  where x' = x {outcome=Nothing}
        y' = y {outcome=Nothing}


-- | The initial sequences stored at birth are genetically determined.
instance (Genetic a) => Genetic (Response a) where
  put (Response s a o)
    = put s >> put a >> put (fmap (scaleToWord8 outcomeInterval) o)
  get = do
    s <- get
    a <- get
    o <- get :: Reader (Either [String] (Maybe Word8))
    return $ Response <$> s <*> a <*> fmap (fmap $ scaleFromWord8 outcomeInterval) o

instance (Diploid a) => Diploid (Response a)

instance (Show a) => Pretty (Response a) where
  pretty (Response s a o)
    = pretty s ++ '|':show a ++ '|':format o
    where format (Just x) = printf "%.3f" x
          format _ = "ø"

-- | Returns a random response model for a decider that operates with a
--   classifier containing the specified number of models.
--   This is useful for generating random deciders.
randomResponse
  :: (RandomGen g, Random a)
    => Int -> Rand g (Response a)
randomResponse n
  = Response <$> randomScenario n <*> getRandom <*> fmap Just (getRandomR outcomeInterval)

-- | Updates the outcome in the second response to match the first.
copyOutcomeTo :: Response a -> Response a -> Response a
copyOutcomeTo source target = target { outcome=outcome source }

-- | Updates the outcome in a response model.
setOutcome :: Response a -> Double -> Response a
setOutcome r o = r { outcome=Just o }

-- getOutcome :: Response s a -> Double
-- getOutcome = outcome
