------------------------------------------------------------------------ 
-- |
-- Module      :  ALife.Creatur.Wain.Condition
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A wain's "condition" includes anything it needs to know about its
-- current status in order to make good decisions.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Condition
  (
    Condition(..),
    cEnergy,
    cPassion,
    cLitterSize,
    randomCondition,
    happiness,
    conditionDiff,
    makeConditionSimilar
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.Util (unitInterval, scaleFromWord8,
  scaleToWord8, doubleTo8BitHex)
import ALife.Creatur.Wain.Weights (Weights, toDoubles)
import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad.Random (Rand, RandomGen, getRandom)
import Data.Datamining.Pattern (adjustNum)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Printf (printf)

-- | A model of a stimulus and the response to it
data Condition = Condition
  {
    -- | Current energy level
    _cEnergy :: Double,
    -- | Current passion level
    _cPassion :: Double,
    -- | Is the wain currently rearing a litter?
    _cLitterSize :: Word8
  } deriving (Eq, Show, Generic, Ord)
makeLenses ''Condition

instance Serialize Condition

-- An agent's current condition is not genetic, of course. However,
-- the initial response patterns stored at birth are genetically
-- determined, and they contain condition information.
instance Genetic Condition where
  put (Condition e p l) = do
    G.put $ scaleToWord8 unitInterval e
    G.put $ scaleToWord8 unitInterval p
    G.put l
  get = do
    e <- fmap (fmap $ scaleFromWord8 unitInterval) G.get
    p <- fmap (fmap $ scaleFromWord8 unitInterval) G.get
    l <- G.get
    return $ Condition <$> e <*> p <*> l

instance Diploid Condition

-- | Wains seek to maximise their happiness, which is a function
--   their current condition.
happiness :: Weights -> Condition -> Double
happiness w (Condition e p l)
  = sum (zipWith (*) ws ds)
  where ds = [e, p, l']
        l' = if l > 0 then 1 else 0
        ws = toDoubles w

conditionDiff
  :: Weights -> Condition -> Condition -> Double
conditionDiff w x y
  = sum (zipWith (*) ws ds)
  where eDiff = abs (_cEnergy x - _cEnergy y)
        pDiff = abs (_cPassion x - _cPassion y)
        lDiff = if _cLitterSize x == _cLitterSize y then 0 else 1
        ds = [eDiff, pDiff, lDiff]
        ws = toDoubles w

makeConditionSimilar :: Condition -> Double -> Condition -> Condition
makeConditionSimilar target r x = Condition e p l
  where e = adjustNum (_cEnergy target) r (_cEnergy x)
        p = adjustNum (_cPassion target) r (_cPassion x)
        l = round $ adjustNum lTarget r lX
        lTarget = fromIntegral . _cLitterSize $ target :: Double
        lX = fromIntegral . _cLitterSize $ x :: Double

-- | Returns a random condition.
--   This is useful for generating random response models.
randomCondition :: RandomGen g => Rand g Condition
randomCondition = do
    e <- getRandom
    p <- getRandom
    l <- getRandom
    return $ Condition e p l

instance Pretty Condition where
  pretty (Condition e p l)
    = doubleTo8BitHex e ++ doubleTo8BitHex p ++ printf "%.2X" l
