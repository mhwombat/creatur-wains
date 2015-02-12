------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ResponseInternal
-- Copyright   :  (c) Amy de Buitléir 2013-2015
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.ResponseInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic, Reader, put, get)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.Scenario (Scenario, randomScenario,
  scenarioDiff, makeScenarioSimilar)
import ALife.Creatur.Wain.Util (scaleFromWord8, scaleToWord8)
import ALife.Creatur.Wain.Weights (Weights, toDoubles)
import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad.Random (Rand, RandomGen, getRandom, getRandomR)
import Data.Datamining.Pattern (adjustNum)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)
import System.Random (Random)
import Text.Printf (printf)

outcomeInterval :: (Double, Double)
outcomeInterval = (-1.0,1.0)

-- | A model of a situation that a wain might encounter.
data Response a = Response
  {
    -- | The situation to be responded to
    _scenario :: Scenario,
    -- | Action
    _action :: a,
    -- | Happiness level change (predicted or actual).
    --   Response patterns stored in the decider will have a @Just@
    --   value; stimuli received from the outside will have a @Nothing@
    --   value.
    _outcome :: Maybe Double
  } deriving (Eq, Show, Generic, Ord)
makeLenses ''Response

instance (Serialize a) => Serialize (Response a)
instance (Diploid a) => Diploid (Response a)

-- | The initial sequences stored at birth are genetically determined.
instance (Genetic a) => Genetic (Response a) where
  put (Response s a o)
    = put s >> put a >> put (fmap (scaleToWord8 outcomeInterval) o)
  get = do
    s <- get
    a <- get
    o <- get :: Reader (Either [String] (Maybe Word8))
    return $ Response <$> s <*> a <*> fmap (fmap $ scaleFromWord8 outcomeInterval) o

instance (Show a) => Pretty (Response a) where
  pretty (Response s a o)
    = pretty s ++ '|':show a ++ '|':format o
    where format (Just x) = printf "%.3f" x
          format _ = "ø"

responseDiff
  :: Eq a
    => Weights -> Weights -> Weights -> Response a -> Response a -> Double
responseDiff cw sw rw x y =
    if _action x == _action y
      then sum (zipWith (*) ws ds) / 2
      else 1.0
    where ds = [sDiff, rDiff]
          sDiff = scenarioDiff cw sw (_scenario x) (_scenario y)
          rDiff = outcomeDiff (fromMaybe 0 . _outcome $ x)
                    (fromMaybe 0 . _outcome $ y)
          ws = toDoubles rw

diffIgnoringOutcome
  :: Eq a
    => Weights -> Weights -> Weights -> Response a -> Response a -> Double
diffIgnoringOutcome cw sw rw x y = responseDiff cw sw rw x' y'
  where x' = set outcome Nothing x
        y' = set outcome Nothing y

makeResponseSimilar
  :: Eq a
    => Response a -> Double -> Response a -> Response a
makeResponseSimilar target r x =
    if _action target == _action x
       then Response s a o
       else x
    where s = makeScenarioSimilar (_scenario target) r (_scenario x)
          a = _action x
          o = Just $ adjustNum (fromMaybe 0.0 . _outcome $ target) r
                (fromMaybe 0.0 . _outcome $ x)

outcomeDiff :: Double -> Double -> Double
outcomeDiff a b = abs (a-b) / 2
  -- a and b are in the interval [-1,1]. We want the difference to be
  -- in the interval [0,1]

similarityIgnoringOutcome
  :: Eq a
    => Weights -> Weights -> Weights -> Response a -> Response a -> Double
similarityIgnoringOutcome cw sw rw x y
  = 1 - diffIgnoringOutcome cw sw rw x y

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
copyOutcomeTo source = set outcome (_outcome source)

-- | Updates the outcome in a response model.
setOutcome :: Response a -> Double -> Response a
setOutcome r o = set outcome (Just o) r

-- getOutcome :: Response s a -> Double
-- getOutcome = outcome
