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
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module ALife.Creatur.Wain.ResponseInternal where

import           ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import           ALife.Creatur.Genetics.Diploid   (Diploid)
import           ALife.Creatur.Wain.Classifier    (Label)
import           ALife.Creatur.Gene.Numeric.PlusMinusOne
    (PM1Double, forceDoubleToPM1, pm1ToDouble)
import           ALife.Creatur.Wain.Pretty        (Pretty, pretty)
import           ALife.Creatur.Wain.Statistics    (Statistical (..), dStat)
import           ALife.Creatur.Gene.Numeric.UnitInterval
    (UIDouble, doubleToUI)
import           Control.DeepSeq                  (NFData)
import           Control.Lens
import           Data.List                        (intercalate)
import           Data.Serialize                   (Serialize)
import           GHC.Generics                     (Generic)
import           Test.QuickCheck                  (arbitrary, choose, Gen, vectorOf)
import           Text.Printf                      (printf)

-- | A model of a scenario that a wain might encounter
data Response a = Response
  {
    -- | The classifier labels for the objects we're responding to
    _labels   :: [Label],
    -- | Action
    _action   :: a,
    -- | Happiness level change (predicted or actual)
    _outcomes :: [PM1Double]
  } deriving ( Eq, Show, Read, Generic, Ord, Serialize, Diploid,
               NFData )
makeLenses ''Response

instance (Genetic a) => Genetic (Response a)

instance (Pretty a) => Pretty (Response a) where
  pretty (Response ls a os) =
    intercalate "|" (map show ls) ++ '|':pretty a ++ '|':format os
    where format xs =  intercalate "|" . map (printf "%.3f" .  pm1ToDouble) $ xs

-- | Internal method
labelSimilarity :: [Label] -> [Label] -> UIDouble
labelSimilarity xs ys =
  if count == 0
    then 1
    else doubleToUI $ fromIntegral matchCount / fromIntegral count
  where comparisons =  labelSimilarity' xs ys
        count = length comparisons
        matchCount = length $ filter id comparisons

-- | Internal method
labelSimilarity' :: [Label] -> [Label] -> [Bool]
labelSimilarity' (x:xs) (y:ys) = (x == y) : (labelSimilarity' xs ys)
labelSimilarity' (_:xs) []     = False : (labelSimilarity' xs [])
labelSimilarity' [] (_:ys)     = False : (labelSimilarity' [] ys)
labelSimilarity' [] []         = []

-- | Updates the outcomes in the second response to match the first.
copyOutcomesTo :: Response a -> Response a -> Response a
copyOutcomesTo source = set outcomes (_outcomes source)

-- | Increment the outcomes in a response by the specified amount.
--   Note: Outcomes are capped at 1.
addToOutcomes :: [PM1Double] -> Response a -> Response a
addToOutcomes deltas r = set outcomes ys r
  where xs = map pm1ToDouble . _outcomes $ r
        deltas' = map pm1ToDouble deltas
        ys = map forceDoubleToPM1 $ zipWith (+) xs deltas'

instance (Statistical a)
    => Statistical (Response a) where
  stats r = (stats . _action $ r)
              ++ [dStat "Δe" (head os),
                  dStat "Δp" (os !! 1),
                  dStat "Δb" (os !! 2),
                  dStat "Δl" (os !! 3)]
    where os = _outcomes r

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

