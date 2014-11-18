------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Checkpoint
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Constraints that can be evaluated on or after a certain time.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Checkpoint
  (
    Checkpoint(..),
    Limit(..),
    enforceAll
  ) where

import Prelude hiding (lookup)
import ALife.Creatur.Task (requestShutdown)
import ALife.Creatur.Universe (Universe, currentTime)
import ALife.Creatur.Wain.Statistics (Statistic, lookup)
import Control.Monad.State.Lazy (StateT)
import Control.Monad (when)

-- | @`Check t s l`@ creates a constraint @l@ on the statistic @s@
--   to be satisfied beginning at time @t@.
data Checkpoint = Check Int String Limit deriving (Show, Read)

-- | A constraint to be evaluated. A constraint is either satisfied
--   or fails. Constraints can either be @`In`@ a range,
--   @`GE'@ (greater than or equal to) a value, or
--   @`LE'@ (less than or equal to) a value.
data Limit = In (Double, Double) | GE Double | LE Double
  deriving (Show, Read)

satisfies :: Double -> Limit -> Bool
satisfies v (In (a,b)) = a <= v && v <= b
satisfies v (GE x) = v >= x
satisfies v (LE x) = v <= x

fails :: Double -> Limit -> Bool
fails v l = not $ v `satisfies` l

-- | Enforce a set of checkpoints. If any of the checkpoints fail,
--   shut down the daemon.
enforceAll
  :: (Universe u)
    => [Statistic] -> [Checkpoint] -> StateT u IO ()
enforceAll xs cs = mapM_ (enforce xs) cs

enforce
  :: (Universe u)
    => [Statistic] -> Checkpoint -> StateT u IO ()
enforce xs c@(Check start key lim) = do
  t <- currentTime
  case lookup key xs of
    Just x -> when (t >= start && x `fails` lim) $ requestShutdown 
               ("failed check " ++ show c ++ " " ++ key ++ "=" ++ show x)
    Nothing -> requestShutdown $ "Cannot find statistic: " ++ key
