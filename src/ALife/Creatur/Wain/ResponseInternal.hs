------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ResponseInternal
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
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
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
module ALife.Creatur.Wain.ResponseInternal where

import qualified ALife.Creatur.Gene.Numeric.PlusMinusOne as PM1
import           ALife.Creatur.Gene.Numeric.UnitInterval (UIDouble, narrow)
import           ALife.Creatur.Genetics.BRGCWord8        (Genetic)
import           ALife.Creatur.Genetics.Diploid          (Diploid)
import           ALife.Creatur.Wain.GeneticSOM           (Label)
import           ALife.Creatur.Wain.Pretty               (Pretty, pretty)
import           ALife.Creatur.Wain.Statistics           (Statistical (..),
                                                          dStat)
import           Control.DeepSeq                         (NFData)
import           Data.List                               (intercalate)
import           Data.Serialize                          (Serialize)
import           GHC.Generics                            (Generic)
import           Test.QuickCheck                         (Gen, arbitrary,
                                                          choose, vectorOf)
import           Text.Printf                             (printf)

-- | A model of a scenario that a wain might encounter
data Response a = Response
  {
    -- | The classifier labels for the objects we're responding to
    labels   :: [Label],
    -- | Action
    action   :: a,
    -- | Happiness level change (predicted or actual)
    outcomes :: [PM1.PM1Double]
  } deriving ( Eq, Show, Read, Generic, Ord, Serialize, Diploid,
               NFData )

instance (Genetic a) => Genetic (Response a)

instance (Pretty a) => Pretty (Response a) where
  pretty (Response ls a os) =
    intercalate "|" (map show ls) ++ '|':pretty a ++ '|':format os
    where format xs =  intercalate "|" . map (printf "%.3f" .  PM1.wide) $ xs

-- | Internal method
labelSimilarity :: [Label] -> [Label] -> UIDouble
labelSimilarity xs ys =
  if count == 0
    then 1
    else narrow $ fromIntegral matchCount / fromIntegral count
  where comparisons =  labelSimilarity' xs ys
        count = length comparisons
        matchCount = length $ filter id comparisons

-- | Internal method
labelSimilarity' :: [Label] -> [Label] -> [Bool]
labelSimilarity' (x:xs) (y:ys) = (x == y) : labelSimilarity' xs ys
labelSimilarity' (_:xs) []     = False : labelSimilarity' xs []
labelSimilarity' [] (_:ys)     = False : labelSimilarity' [] ys
labelSimilarity' [] []         = []

-- | Updates the outcomes in the second response to match the first.
copyOutcomesTo :: Response a -> Response a -> Response a
copyOutcomesTo source r = r { outcomes=outcomes source }

-- | Increment the outcomes in a response by the specified amount.
--   Note: Outcomes are capped at 1.
addToOutcomes :: [PM1.PM1Double] -> Response a -> Response a
addToOutcomes deltas r = r { outcomes=ys }
  where xs = map PM1.wide . outcomes $ r
        deltas' = map PM1.wide deltas
        ys = map PM1.crop $ zipWith (+) xs deltas'

instance (Statistical a)
    => Statistical (Response a) where
  stats r = (stats . action $ r)
              ++ [dStat "Δe" (head os),
                  dStat "Δp" (os !! 1),
                  dStat "Δb" (os !! 2),
                  dStat "Δl" (os !! 3)]
    where os = outcomes r

-- This method is used by other test classes to ensure that all of the
-- scenarios have the same number of objects and condition parameters.
sizedArbResponse :: Gen a -> Int -> Gen (Response a)
sizedArbResponse genAction n = do
  nObjects <- choose (0, n)
  let nConditions = n - nObjects
  arbResponse nObjects nConditions genAction

-- This method is used by other test classes to ensure that all of the
-- scenarios have the same number of objects and condition parameters.
arbResponse :: Int -> Int -> Gen a -> Gen (Response a)
arbResponse nObjects nConditions genAction = do
  s <- vectorOf nObjects arbitrary
  a <- genAction
  o <- vectorOf nConditions arbitrary
  return $ Response s a o

