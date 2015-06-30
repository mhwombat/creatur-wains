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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.ResponseInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.Scenario (Scenario, randomScenario,
  scenarioDiff, makeScenarioSimilar)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double,
  pm1ToDouble, adjustPM1Double, pm1Diff)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI,
  uiApply)
import ALife.Creatur.Wain.Util (intersection)
import ALife.Creatur.Wain.Weights (Weights, weightedSum)
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad.Random (Rand, RandomGen, getRandom, getRandomR)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Random (Random)
import Text.Printf (printf)

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
    _outcome :: Maybe PM1Double
  } deriving ( Eq, Show, Read, Generic, Ord, Serialize, Genetic,
               Diploid, NFData )
makeLenses ''Response

instance (Show a) => Pretty (Response a) where
  pretty (Response s a o)
    = pretty s ++ '|':show a ++ '|':format o
    where format (Just x) =  printf "%.3f" $ pm1ToDouble x
          format _ = "ø"

-- | @'responseDiff' cw sw rw x y@ compares the response patterns
--   @x@ and @y@, and returns a number between 0 and 1, representing
--   how different the patterns are. A result of 0 indicates that the
--   patterns are identical.
--   The parameter @cw@ determines the relative weight to assign to
--   differences in energy, passion, and whether or not there is a
--   litter in each pattern.
--   The parameter @sw@ determines the relative weight to assign to
--   differences between each corresponding pair of objects represented
--   by the scenario being responded to in each pattern.
--   The parameter @rw@ determines the relative weight to assign to
--   differences in the scenarios and the outcomes in the two patterns.
responseDiff
  :: Eq a
    => Weights -> Weights -> Weights -> Response a -> Response a
      -> UIDouble
responseDiff cw sw rw x y =
    if _action x == _action y
      then weightedSum rw ds
      else doubleToUI 1.0
    where ds = [sDiff, oDiff]
          sDiff = scenarioDiff cw sw (_scenario x) (_scenario y)
          oDiff = pm1Diff (fromMaybe 0 . _outcome $ x)
                    (fromMaybe 0 . _outcome $ y)

diffIgnoringOutcome
  :: Eq a
    => Weights -> Weights -> Weights -> Response a -> Response a
      -> UIDouble
diffIgnoringOutcome cw sw rw x y = responseDiff cw sw rw x' y'
  where x' = set outcome Nothing x
        y' = set outcome Nothing y

makeResponseSimilar
  :: Eq a
    => Response a -> UIDouble -> Response a -> Response a
makeResponseSimilar target r x =
    if _action target == _action x
       then Response s a o
       else x
    where s = makeScenarioSimilar (_scenario target) r (_scenario x)
          a = _action x
          o = Just $ adjustPM1Double (fromMaybe 0.0 . _outcome $ target)
                r (fromMaybe 0.0 . _outcome $ x)

similarityIgnoringOutcome
  :: Eq a
    => Weights -> Weights -> Weights -> Response a -> Response a
      -> UIDouble
similarityIgnoringOutcome cw sw rw x y
  = uiApply (\z -> 1 - z) $ diffIgnoringOutcome cw sw rw x y

-- | @'randomResponse' n k m@ returns a random response model involving
--   @n@ objects, for a decider that operates with a classifier
--   containing @k@ models, for a wain whose condition involves
--   @m@ elements.
--   This is useful for generating random deciders.
randomResponse
  :: (RandomGen g, Random a)
    => Int -> Int -> Int -> (PM1Double, PM1Double) -> Rand g (Response a)
randomResponse n k m customInterval
  = Response <$> randomScenario n k m <*> getRandom
      <*> fmap Just (getRandomR interval')
  where interval' = intersection (-1, 1) customInterval

-- | Updates the outcome in the second response to match the first.
copyOutcomeTo :: Response a -> Response a -> Response a
copyOutcomeTo source = set outcome (_outcome source)

-- | Updates the outcome in a response model.
setOutcome :: Response a -> PM1Double -> Response a
setOutcome r o = set outcome (Just o) r

-- getOutcome :: Response s a -> Double
-- getOutcome = outcome
