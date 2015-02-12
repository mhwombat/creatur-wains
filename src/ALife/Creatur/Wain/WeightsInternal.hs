------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.WeightsInternal
-- Copyright   :  (c) Amy de Buitléir 2015
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
{-# LANGUAGE DeriveGeneric #-}
module ALife.Creatur.Wain.WeightsInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Wain.Util (unitInterval, scaleFromWord8,
  scaleToWord8)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Weights = Weights [Double]
  deriving (Eq, Show, Generic, Ord)

instance Serialize Weights

instance Genetic Weights where
  put (Weights ws) = G.put . map (scaleToWord8 unitInterval) $ ws
  get = fmap (fmap (Weights . map (scaleFromWord8 unitInterval))) G.get

instance Diploid Weights
  -- sum of weights will never be >1, because "express" chooses the
  -- smaller value

makeWeights :: [Double] -> Weights
makeWeights ws
  | null ws   = Weights []
  | k == 0     = Weights $ replicate n (1 / fromIntegral n)
  | otherwise = Weights $ map (/k) ws'
  where k = sum ws'
        ws' = map abs ws
        n = length ws

toDoubles :: Weights -> [Double]
toDoubles (Weights xs) = xs

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

-- If a weight isn't present in the list, the effect is the same as if
-- the weight were set to 0.
-- Weights are short lists, so the call to length isn't too
-- inefficient.
weightAt :: Weights -> Int -> Double
weightAt w n = if length ws > n
               then ws !! n
               else 0
  where ws = toDoubles w
