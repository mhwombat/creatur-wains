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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module ALife.Creatur.Wain.WeightsInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic, get)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiApply, uiToDouble,
  doubleToUI, uiDiff)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Weights = Weights [UIDouble]
  deriving (Eq, Show, Generic, Ord, Serialize, NFData)
  -- NOTE: Regarding Diploid instance, sum of weights will never be >1,
  -- because "express" chooses the smaller value.

instance Genetic Weights where
  -- use default put
  get = fmap (fmap makeWeights) get

instance Diploid Weights where
  express (Weights xs) (Weights ys) = makeWeights zs
    where zs = express xs ys

makeWeights :: [UIDouble] -> Weights
makeWeights [] = Weights []
makeWeights ws = Weights . normalise $ ws

normalise :: [UIDouble] -> [UIDouble]
normalise ws
  | k == 0    = replicate n (doubleToUI (1 / fromIntegral n))
  | otherwise = tweak $ map (uiApply (/k)) ws
  where k = sum . map uiToDouble $ ws
        n = length ws

tweak :: [UIDouble] -> [UIDouble]
tweak (x:xs) = if excess > 0 then (x - excess):xs else x:xs
  where excess = doubleToUI . max 0 $ s - 1
        s = sum . map uiToDouble $ (x:xs)
tweak [] = error "tweak should not have been called"

numWeights :: Weights -> Int
numWeights (Weights xs) = length xs

weightedSum :: Weights -> [UIDouble] -> UIDouble
weightedSum ws xs = sum $ zipWith (*) (toUIDoubles ws) xs

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

-- If a weight isn't present in the list, the effect is the same as if
-- the weight were set to 0.
-- Weights are short lists, so the call to length isn't too
-- inefficient.
weightAt :: Weights -> Int -> UIDouble
weightAt w n = if length ws > n
               then ws !! n
               else 0
  where ws = toUIDoubles w

weightedUIVectorDiff
  :: Weights -> [UIDouble] -> [UIDouble] -> UIDouble
weightedUIVectorDiff ws xs ys
  = sum . zipWith (*) (toUIDoubles ws) $ zipWith uiDiff xs ys
