------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.WeightsInternal
-- Copyright   :  (c) Amy de Buitléir 2015-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @Weights@ internals. Most developers
-- should use @Weights@ instead. This module is subject to change
-- without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module ALife.Creatur.Wain.WeightsInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic, get)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Wain.Pretty (Pretty)
import ALife.Creatur.Wain.Statistics (Statistical(..), dStats)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiDiff, normalise)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- | A sequence of weights for calculating weighted sums.
data Weights = Weights [UIDouble]
  deriving (Eq, Show, Generic, Ord, Serialize, NFData, Pretty)
  -- NOTE: Regarding Diploid instance, sum of weights will never be >1,
  -- because "express" chooses the smaller value.

instance Genetic Weights where
  -- use default put
  get = fmap (fmap makeWeights) get

instance Diploid Weights where
  express (Weights xs) (Weights ys) = makeWeights zs
    where zs = express xs ys

-- | Constructs a sequence of weights based on the input vector, but
--   normalised so that the sum of the weights is 1.
makeWeights :: [UIDouble] -> Weights
makeWeights [] = Weights []
makeWeights ws = Weights . normalise $ ws

-- | Number of weights in a sequence.
numWeights :: Weights -> Int
numWeights (Weights xs) = length xs

-- | Calculates the weighted sum of a sequence of values.
weightedSum :: Weights -> [UIDouble] -> UIDouble
weightedSum ws xs = sum $ zipWith (*) (toUIDoubles ws) xs

-- | Extract the weights from a @Weights@ object.
toUIDoubles :: Weights -> [UIDouble]
toUIDoubles (Weights xs) = xs

-- randomWeights :: RandomGen g => Int -> Rand g Weights
-- randomWeights n = do
--   xs <- fmap (take n) $ getRandomRs unitInterval
--   let s = sum xs
--   if s == 0
--     then return . Weights $ xs
--     else do
--       let ys = map (/s) xs
--       let zs = map (roundtripToWord8 unitInterval) $ ys
--       if sum zs > 1
--         then randomWeights n -- try again
--         else return $ Weights zs

-- | Returns the weight at a specified index in the sequence,
--   or zero if there is no weight at that index.
-- Weights are short lists, so the call to length isn't too
-- inefficient.
weightAt :: Weights -> Int -> UIDouble
weightAt w n = if length ws > n
               then ws !! n
               else 0
  where ws = toUIDoubles w

-- | Calculates the weighted difference between two sequences of
--   numbers.
--   Returns a number between 0 and 1.
--   A result of 0 indicates that the inputs are identical.
weightedUIVectorDiff
  :: Weights -> [UIDouble] -> [UIDouble] -> UIDouble
weightedUIVectorDiff ws xs ys
  = sum . zipWith (*) (toUIDoubles ws) $ zipWith uiDiff xs ys

instance Statistical Weights where
  stats = dStats "" . toUIDoubles
