------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.GeneticSOMInternal
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private GeneticSOM internals.
-- Most developers should use GeneticSOM instead.
-- This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.GeneticSOMInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Wain.Statistics (Statistical, iStat, stats)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI,
  uiToDouble)
import ALife.Creatur.Wain.Util (intersection)
import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad.Random (Rand, RandomGen, getRandomR)
import Data.Datamining.Pattern (Pattern, Metric)
import qualified Data.Datamining.Clustering.Classifier as C
import qualified Data.Datamining.Clustering.SSOM as SOM
import Data.Datamining.Clustering.SSOMInternal (rate)
import qualified Data.Map.Strict as M
import qualified Data.Serialize as S
import Data.Word (Word16)
import GHC.Generics (Generic)

type Label = Word16

instance S.Serialize (SOM.Exponential Double)

instance Genetic (SOM.Exponential Double) where
  put (SOM.Exponential r0 d) = do
    G.put $ doubleToUI r0
    G.put $ doubleToUI d
  get = do
    r0 <- G.get :: G.Reader (Either [String] UIDouble)
    d <- G.get :: G.Reader (Either [String] UIDouble)
    return $ SOM.Exponential <$> fmap uiToDouble r0
      <*> fmap uiToDouble d

instance (Diploid a) => Diploid (SOM.Exponential a)

data RandomExponentialParams = RandomExponentialParams
  {
    _r0Range :: (Double, Double),
    _dRange :: (Double, Double)
  } deriving Show

makeLenses ''RandomExponentialParams

r0RangeLimits :: (Double, Double)
r0RangeLimits = (1/255, 1)

dRangeLimits :: (Double, Double)
dRangeLimits = (1/255, 1)

-- | Returns a set of parameters which will permit the broadest possible
--   set of random decaying gaussian functions for a SOM.
randomExponentialParams :: RandomExponentialParams
randomExponentialParams =
  RandomExponentialParams r0RangeLimits dRangeLimits

-- | @'randomExponential' r0Range dRange@ returns a random decaying
--   exponential that can be used as the learning function for a SOM.
--   The parameters of the gaussian will be chosen such that:
--
--   * r0 is in the /intersection/ of the range r0Range and (0, 1)
--   * d is in the /intersection/ of the range dRange and (0, 1)
randomExponential
  :: RandomGen g
    => RandomExponentialParams
      -> Rand g (SOM.Exponential Double)
randomExponential p = do
  r0 <- getRandomR . intersection r0RangeLimits . _r0Range $ p
  d <- getRandomR . intersection dRangeLimits . _dRange $ p
  return $ SOM.Exponential r0 d

validExponential :: SOM.Exponential Double -> Bool
validExponential (SOM.Exponential r0 d) = 0 <= r0 && r0 <= 1
                                            && 0 <= d && d <= 1

instance (Genetic k, Ord k, Genetic p) => Genetic (M.Map k p) where
  put gm = G.put (M.toList gm)
  get = do
    xs <- G.get
    return $ M.fromList <$> xs
instance (Ord k, Diploid p) => Diploid (M.Map k p) where
  express gm1 gm2 = M.fromList . zip ks $ vs
    where ks = M.keys gm1
          vs = express (M.elems gm1) (M.elems gm2)

instance
  (S.Serialize f, S.Serialize t, S.Serialize k, Ord k, S.Serialize p) =>
    S.Serialize (SOM.SSOM f t k p)
instance (Genetic f, Genetic t, Genetic k, Ord k, Genetic p) =>
  Genetic (SOM.SSOM f t k p)
instance (Diploid f, Diploid t, Diploid k, Ord k, Diploid p) =>
  Diploid (SOM.SSOM f t k p)

data GeneticSOM p =
  GeneticSOM
    {
      _patternMap :: SOM.SSOM (SOM.Exponential Double) Word16 Label p,
      _counterMap :: M.Map Label Word16
    }
  deriving (Eq, Show, Generic)

makeLenses ''GeneticSOM

-- | Returns @True@ if the SOM has a valid Exponential and at least one
--   model; returns @False@ otherwise.
somOK
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> Bool
somOK s
  = (not . null . models $ s) && (numModels s > 1)
      && (validExponential . learningFunction $ s)

-- | @'buildGeneticSOM' f ps@ returns a genetic SOM, using the learning
--   function @f@, and initialised with the models @ps@.
buildGeneticSOM
  :: (Pattern p, Metric p ~ Double)
    => SOM.Exponential Double -> [p] -> GeneticSOM p
buildGeneticSOM f@( SOM.Exponential r0 d) xs
  | null xs   = error "SOM has no models"
  | r0 < 0    = error "r0<0"
  | d < 0     = error "d<0"
  | otherwise = GeneticSOM som ks
  where gm = M.fromList . zip [0..] $ xs
        zeros = map (const 0) xs
        som = SOM.SSOM gm f 0
        ks = M.fromList . zip [0..] $ zeros

instance (S.Serialize p) => S.Serialize (GeneticSOM p)

instance (Genetic p) => Genetic (GeneticSOM p)
-- instance (Genetic p) => Genetic (GeneticSOM p) where
--   put = G.put . patternMap
--   get = do
--     som <- G.get
--     case som of
--       Right s ->
--         if (Grid.null . gridMap $ patternMap s)
--           then return $ Left ["Genetic SOM has no models"]
--           else return $ Right s
--       Left ms -> return $ Left ms

instance (Diploid p) => Diploid (GeneticSOM p)

instance (Pattern p, Metric p ~ Double) => Statistical (GeneticSOM p) where
  stats s =
    (iStat "num models" . numModels $ s)
      :(stats . SOM.learningFunction . _patternMap $ s)

-- | Adjusts the model at the index (grid location) specified.
--   Only the one model is changed. This is useful for allowing wains
--   to learn from each other.
-- TODO: Maybe it would be better to do the normal training instead?
learn
  :: (Pattern p, Metric p ~ Double)
    => p -> Label -> GeneticSOM p -> GeneticSOM p
learn p l s = set patternMap gm' s
  where gm = _patternMap s
        gm' = SOM.trainNode gm l p

-- | Returns the index (grid location) of the model that most closely
--   matches the input pattern. The SOM is not updated.
justClassify
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> p -> Label
justClassify s = C.classify (_patternMap s)

-- | Updates the SOM based on the input pattern.
--   Returns the index (grid location) of the model that most closely
--   matches the input pattern, the differences between the input
--   pattern and each model in the SOM, the novelty of the input
--   pattern, the adjusted novelty, and the updated SOM (the counter
--   for the closest model is incremented).
reportAndTrain
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> p
      -> (Label, [(Label, Metric p)], GeneticSOM p)
reportAndTrain s p = (bmu, diffs, s')
  where (bmu, diffs, som') = C.reportAndTrain (_patternMap s) p
        cMap = M.adjust (+1) bmu (_counterMap s)
        s' = set patternMap som' . set counterMap cMap $ s

-- | Returns the number of models in the SOM.
numModels
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> Int
numModels (GeneticSOM s _) = C.numModels s

-- | Returns the models in the SOM.
models
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> [p]
models (GeneticSOM s _) = C.models s

modelAt :: GeneticSOM p -> Label -> p
modelAt s k = SOM.toMap (_patternMap s) M.! k

-- | Returns a list containing each index (grid location) in the SOM,
--   paired with the model at that index.
toList :: (Pattern p, Metric p ~ Double) => GeneticSOM p -> [(Label, p)]
toList (GeneticSOM s _) = C.toList s

learningFunction :: GeneticSOM p -> SOM.Exponential Double
learningFunction (GeneticSOM s _) = SOM.learningFunction s

currentLearningRate :: GeneticSOM p -> Double
currentLearningRate s
  = rate (learningFunction s) (fromIntegral t)
  where t = SOM.counter . _patternMap $ s

schemaQuality :: GeneticSOM p -> Int
schemaQuality = discrimination . M.elems . _counterMap

discrimination :: Integral a => [a] -> Int
discrimination xs = length $ filter (>k) xs
  where k = sum xs `div` fromIntegral (2 * length xs)
