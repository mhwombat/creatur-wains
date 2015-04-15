------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Scenario
-- Copyright   :  (c) Amy de Buitléir 2013-2015
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
    randomScenario,
    scenarioDiff,
    makeScenarioSimilar
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic, put, get,
  putRawWord8s, getRawWord8s)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Condition (Condition, randomCondition,
  conditionDiff, makeConditionSimilar)
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.UnitInterval (vectorDiff)
import ALife.Creatur.Wain.Weights (Weights, toDoubles)
import ALife.Creatur.Wain.Util (forceIntToWord8, word8ToInt,
  scaleToWord8, scaleFromWord8, unitInterval, doublesTo8BitHex)
import Control.Lens
import Control.Monad (replicateM)
import Control.Monad.Random (Rand, RandomGen, getRandom)
import Data.Datamining.Pattern (adjustVector)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)

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

scenarioDiff :: Weights -> Weights -> Scenario -> Scenario -> Double
scenarioDiff conditionWeights scenarioWeights x y = sum (zipWith (*) ws ds)/3
  where ds = [doDiff, ioDiff, cDiff]
        doDiff = vectorDiff (_directObject x)   (_directObject y)
        ioDiff = vectorDiff (_indirectObject x) (_indirectObject y)
        cDiff = conditionDiff conditionWeights (_condition x) (_condition y)
        ws = toDoubles scenarioWeights

makeScenarioSimilar :: Scenario -> Double -> Scenario -> Scenario
makeScenarioSimilar target r x = Scenario dObj iObj cond
  where dObj = adjustVector (_directObject target) r (_directObject x)
        iObj = adjustVector (_indirectObject target) r
                 (_indirectObject x)
        cond = makeConditionSimilar (_condition target) r (_condition x)
    
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

