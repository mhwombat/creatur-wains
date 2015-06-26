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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Scenario
  (
    Scenario(..),
    condition,
    diffs,
    randomScenario,
    scenarioDiff,
    makeScenarioSimilar
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiDoublesTo8BitHex,
  uiVectorDiff, adjustUIVectorPreserveLength)
import ALife.Creatur.Wain.Weights (Weights, weightedUIVectorDiff,
  weightedSum)
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad (replicateM)
import Control.Monad.Random (Rand, RandomGen, getRandom, getRandoms)
import Data.List (intercalate)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- | A wain's assessment of a situation.
data Scenario = Scenario
  {
    -- | The pattern probabilities identified by the classifier
    --   for all of the diffs it evaluated.
    _diffs :: [[UIDouble]],
    -- | Current condition
    _condition :: [UIDouble]
  } deriving ( Eq, Show, Read, Generic, Ord, Serialize, Genetic,
               Diploid, NFData )
makeLenses ''Scenario

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
scenarioDiff :: Weights -> Weights -> Scenario -> Scenario -> UIDouble
scenarioDiff cw sw x y = weightedSum sw ds
  where ds = cDiff:oDiffs
        oDiffs = zipWith uiVectorDiff (_diffs x) (_diffs y)
        cDiff = weightedUIVectorDiff cw (_condition x) (_condition y)

makeScenarioSimilar :: Scenario -> UIDouble -> Scenario -> Scenario
makeScenarioSimilar target r x = Scenario xs cond
  where xs = zipWith (`adjustUIVectorPreserveLength` r)
               (_diffs target) (_diffs x)
        cond = adjustUIVectorPreserveLength
                 (_condition target) r (_condition x)

-- | @'randomScenario' n k m@ returns a random scenario involving @n@
--   objects, for a decider that operates with a classifier containing
--   @k@ models, for a wain whose condition involves @m@ elements.
--   This is useful for generating random responses.
randomScenario :: RandomGen g => Int -> Int -> Int -> Rand g Scenario
randomScenario n k m = do
  xs <- replicateM n (replicateM k getRandom)
  c <- fmap (take m ) getRandoms
  return $ Scenario xs c

instance Pretty Scenario where
  pretty (Scenario xs c)
    = intercalate "|" (map uiDoublesTo8BitHex xs)
        ++ '|':uiDoublesTo8BitHex c
