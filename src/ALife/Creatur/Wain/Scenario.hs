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
{-# LANGUAGE CPP #-}
module ALife.Creatur.Wain.Scenario
  (
    Scenario(..),
    condition,
    diffs,
    classifications,
    randomScenario,
    scenarioDiff,
    makeScenarioSimilar
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic, put, get)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Condition (Condition, randomCondition,
  conditionDiff, makeConditionSimilar)
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.UnitInterval (vectorDiff)
import ALife.Creatur.Wain.Weights (Weights, toDoubles)
import ALife.Creatur.Wain.Util (scaleToWord8, scaleFromWord8,
  unitInterval, uiDoublesTo8BitHex)
import Control.Lens
import Control.Monad (replicateM)
import Control.Monad.Random (Rand, RandomGen, getRandom)
import Data.Datamining.Pattern (adjustVectorPreserveLength)
import Data.List (intersperse, maximumBy)
import Data.Ord (comparing)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif

-- | A wain's assessment of a situation.
data Scenario = Scenario
  {
    -- | The pattern probabilities identified by the classifier
    --   for all of the diffs it evaluated.
    _diffs :: [[Double]],
    -- | Current condition
    _condition :: Condition
  } deriving (Eq, Show, Read, Generic, Ord)

makeLenses ''Scenario

instance Serialize Scenario

-- | For each object in the scenario, returns the index of the model
--   that best matches it.
classifications :: Scenario -> [Int]
classifications = map classification . _diffs

classification :: [Double] -> Int
classification ds = snd $ maximumBy (comparing fst) xs
  where xs = zip ds [0..]

-- | @'scenarioDiff' cw sw x y@ compares the scenario patterns
--   @x@ and @y@, and returns a number between 0 and 1, representing
--   how different the patterns are. A result of 0 indicates that the
--   patterns are identical.
--   The parameter @cw@ determines the relative weight to assign to
--   differences in energy, passion, and whether or not there is a
--   litter in each pattern.
--   The parameter @sw@ determines the relative weight to assign to
--   differences between each corresponding pair of objects represented
--   by the scenarios in each pattern.
scenarioDiff :: Weights -> Weights -> Scenario -> Scenario -> Double
scenarioDiff cw sw x y = sum (zipWith (*) ws ds)
  where ds = cDiff:oDiffs
        oDiffs = zipWith vectorDiff (_diffs x)   (_diffs y)
        cDiff = conditionDiff cw (_condition x) (_condition y)
        ws = toDoubles sw

makeScenarioSimilar :: Scenario -> Double -> Scenario -> Scenario
makeScenarioSimilar target r x = Scenario xs cond
  where xs = zipWith (flip adjustVectorPreserveLength r)
               (_diffs target) (_diffs x)
        cond = makeConditionSimilar (_condition target) r (_condition x)

-- | The initial sequences stored at birth are genetically determined.
instance Genetic Scenario where
  put (Scenario xs c) = do
    put $ map (map (scaleToWord8 unitInterval)) xs
    put c
  get = do
    xs <- fmap (fmap (map (map (scaleFromWord8 unitInterval)))) get
    c <- get
    return $ Scenario <$> xs <*> c

instance Diploid Scenario

-- | @'randomScenario' n k@ returns a random scenario involving @n@
--   objects, for a decider that operates with a classifier containing
--   @k@ models.
--   This is useful for generating random responses.
randomScenario :: RandomGen g => Int -> Int -> Rand g Scenario
randomScenario n k = do
  xs <- replicateM n (replicateM k getRandom)
  c <- randomCondition
  return $ Scenario xs c

instance Pretty Scenario where
  pretty (Scenario xs c)
    = (concat . intersperse "|" $ map uiDoublesTo8BitHex xs)
        ++ '|':pretty c
