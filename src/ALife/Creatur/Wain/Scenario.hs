------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Scenario
-- Copyright   :  (c) Amy de Buitléir 2013-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Scenario
  (
    Scenario(..),
    condition,
    directObject,
    indirectObject,
    randomScenario
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic, put, get,
  putRawWord8s, getRawWord8s)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Condition (Condition, randomCondition)
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.Util (forceIntToWord8, word8ToInt,
  scaleToWord8, scaleFromWord8, unitInterval, doublesTo8BitHex)
import Control.Applicative
import Control.Lens
import Control.Monad (replicateM)
import Control.Monad.Random (Rand, RandomGen, getRandom)
import Data.Datamining.Pattern (Pattern, Metric, difference,
  makeSimilar)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)

-- Weights to use when comparing possible courses of action.
-- The values should add up to one.
-- TODO: Make genetic
scenarioWeights :: [Double]
scenarioWeights =
  [
    0.5, -- direct object signature
    0.3, -- indirect object signature
    0.2 -- condition
  ]

-- | A wain's assessment of a situation.
data Scenario = Scenario
  {
    -- | The pattern probabilities identified by the classifier
    --   for the direct object of the action.
    _directObject :: [Double],
    -- | The pattern probabilities identified by the classifier
    --   for the indirect object of the action.
    _indirectObject :: [Double],
    -- | Current condition
    _condition :: Condition
  } deriving (Eq, Show, Generic, Ord)

makeLenses ''Scenario

instance Serialize Scenario

instance Pattern Scenario where
  type Metric Scenario = Double
  difference x y = sum (zipWith (*) scenarioWeights ds)/3
    where ds = [doDiff, ioDiff, cDiff]
          doDiff = difference (_directObject x)   (_directObject y)
          ioDiff = difference (_indirectObject x) (_indirectObject y)
          cDiff = difference (_condition x) (_condition y)
  makeSimilar target r x = Scenario dObj iObj cond
    where dObj = makeSimilar (_directObject target) r (_directObject x)
          iObj = makeSimilar (_indirectObject target)
                   r (_indirectObject x)
          cond = makeSimilar (_condition target) r (_condition x)
    
-- | The initial sequences stored at birth are genetically determined.
instance Genetic Scenario where
  put (Scenario ds is c) = do
    -- ds and is should have the same length
    put . forceIntToWord8 $ length ds
    putRawWord8s $ doublesToWord8s ds
    putRawWord8s $ doublesToWord8s is
    put c
  get = do
    n <- fmap (fmap word8ToInt) get
    case n of
      Left msgs -> return $ Left msgs
      Right n' -> do
        ds <- fmap word8sToDoubles <$> getRawWord8s n'
        is <- fmap word8sToDoubles <$> getRawWord8s n'
        c <- get
        return $ Scenario <$> ds <*> is <*> c

doublesToWord8s :: [Double] -> [Word8]
doublesToWord8s = map (scaleToWord8 unitInterval)
-- doublesToWord8s = map (round . (255*))

word8sToDoubles :: [Word8] -> [Double]
word8sToDoubles = map (scaleFromWord8 unitInterval)
-- word8sToDoubles = map ((/255) . fromIntegral)

instance Diploid Scenario

-- | Returns a random scenario for a decider that operates with a
--   classifier containing the specified number of models.
--   This is useful for generating random responses.
randomScenario :: RandomGen g => Int -> Rand g Scenario
randomScenario n = do
  ds <- replicateM n getRandom
  is <- replicateM n getRandom
  c <- randomCondition
  return $ Scenario ds is c

instance Pretty Scenario where
  pretty (Scenario d i c)
    = doublesTo8BitHex d ++ '|':doublesTo8BitHex i
      ++ '|':pretty c

