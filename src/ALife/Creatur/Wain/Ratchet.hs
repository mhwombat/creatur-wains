------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Ratchet
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Ratchets can be adjusted in one direction only, within
-- specific limits.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Ratchet
  (
    RatchetSpec(..),
    Ratchet,
    mkRatchet,
    currentValue,
    canAdjust,
    adjust
  ) where

-- | @`RatchetSpec` start stop delta@ specifies a ratchet that starts
--   at the value @start@, and can be adjusted by the value @delta@
--   until it reaches the value @stop@.
--
--   If @start@ <= @stop@, then @delta@ should be non-negative.
--
--   If @start@ >= @stop@, then @delta@ should be non-positive.
data RatchetSpec
  = RatchetSpec Double Double Double
    deriving (Read, Show)

-- | A value that can be adjusted in one direction only, within
--   specific limits.
data Ratchet
  = Ratchet { rStop :: Double,
              rDelta :: Double,
              rCurrent :: Double,
              rGoingUp :: Bool } deriving (Read, Show)

-- | Create a ratchet from a ratchet spec.
mkRatchet :: RatchetSpec -> Either String Ratchet
mkRatchet (RatchetSpec a b d)
  | a <= b && d >= 0 = Right $ Ratchet b d a True
  | a >= b && d <= 0 = Right $ Ratchet b d a False
  | otherwise     = Left "Invalid ratchet spec"

-- \ The current value of the ratchet.
currentValue :: Ratchet -> Double
currentValue = rCurrent

-- | Returns true if the ratchet still has some room for adjustment,
--   otherwise it returns false.
canAdjust :: Ratchet -> Bool
canAdjust r =
  if rGoingUp r
    then rCurrent r < rStop r
    else rCurrent r > rStop r

-- | Sets the ratchet to the next possible value. If the ratchet has
--   no room for adjustment, this function has no effect.
adjust :: Ratchet -> Ratchet
adjust r =
  if rGoingUp r
    then r { rCurrent = min (rStop r) (rCurrent r + rDelta r) }
    else r { rCurrent = max (rStop r) (rCurrent r + rDelta r) }
