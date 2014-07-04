------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.GeneticSOM
-- Copyright   :  (c) Amy de Buitléir 2013-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides a self-organising map that can be genetically configured.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, DeriveGeneric, FlexibleContexts,
    FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.GeneticSOM
  (
    GeneticSOM(..),
    Label,
    buildGeneticSOM,
    numModels,
    models,
    toList,
    justClassify,
    reportAndTrain,
    learn,
    RandomDecayingGaussianParams(..),
    randomDecayingGaussianParams,
    randomDecayingGaussian,
    somOK,
    learningFunction,
    size
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Wain.Statistics (Statistical, iStat, stats)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI,
  uiToDouble)
import ALife.Creatur.Wain.Util (forceIntToWord8, forceIntToWord16,
  intersection)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Random (Rand, RandomGen, getRandomR)
import Data.Datamining.Pattern (Pattern, Metric, makeSimilar)
import qualified Data.Datamining.Clustering.Classifier as C
import qualified Data.Datamining.Clustering.SOM as SOM
-- TODO: Either move currentLearningFunction or "learn" to
-- Data.Datamining.Clustering.SOM
import Data.Datamining.Clustering.SOMInternal (currentLearningFunction)
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Word (Word8, Word16)
import GHC.Generics (Generic)
import qualified Math.Geometry.Grid as Grid
import Math.Geometry.Grid.Hexagonal (hexHexGrid, HexHexGrid)
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMap)

type Label = (Int, Int)

instance Serialize (SOM.DecayingGaussian Double)

instance Genetic (SOM.DecayingGaussian Double) where
  put (SOM.DecayingGaussian r0 rf w0 wf tf) = do
    G.put $ doubleToUI r0
    G.put $ doubleToUI rf
    G.put . forceIntToWord8 $ round w0
    G.put . forceIntToWord8 $ round wf
    G.put . forceIntToWord16 $ round tf
  get = do
    r0 <- G.get :: G.Reader (Either [String] UIDouble)
    rf <- G.get :: G.Reader (Either [String] UIDouble)
    w0 <- G.get :: G.Reader (Either [String] Word8)
    wf <- G.get :: G.Reader (Either [String] Word8)
    tf <- G.get :: G.Reader (Either [String] Word16)
    return $ SOM.DecayingGaussian <$> fmap uiToDouble r0
      <*> fmap uiToDouble rf <*> fmap fromIntegral w0
      <*> fmap fromIntegral wf <*> fmap fromIntegral tf

instance (Diploid a) => Diploid (SOM.DecayingGaussian a)

data RandomDecayingGaussianParams = RandomDecayingGaussianParams
  {
    r0Range :: (Double, Double),
    rfRange :: (Double, Double),
    w0Range :: (Double, Double),
    wfRange :: (Double, Double),
    tfRange :: (Double, Double),
    sideLength :: Word8
  } deriving Show

r0RangeLimits :: (Double, Double)
r0RangeLimits = (1/255, 1)

rfRangeLimits :: Double -> (Double, Double)
rfRangeLimits r0 = (0, r0)

w0RangeLimits :: Word8 -> (Double, Double)
w0RangeLimits s = (1/255, 1 + fromIntegral s)

wfRangeLimits :: Double -> (Double, Double)
wfRangeLimits w0 = (1/255, w0)

tfRangeLimits :: (Double, Double)
tfRangeLimits = (1,fromIntegral tMax)
  where tMax = maxBound :: Word16

-- | Returns a set of parameters which will permit the broadest possible
--   set of random decaying gaussian functions for a SOM that uses a
--   hexagonal grid of size @s@.
randomDecayingGaussianParams :: Word8 -> RandomDecayingGaussianParams
randomDecayingGaussianParams s = 
  RandomDecayingGaussianParams
    {
      r0Range = r0RangeLimits,
      rfRange = r0RangeLimits,
      w0Range = w0RangeLimits s,
      wfRange = w0RangeLimits s,
      tfRange = tfRangeLimits,
      sideLength = s
    }

-- | @'randomDecayingGaussian' r0Range rfRange w0Range wfRange tMaxRange s@
--   returns a random decaying gaussian that can be used as the
--   learning function for a SOM that uses a hexagonal grid of size @s@.
--   The parameters of the gaussian will be chosen such that:
--
--   * r0 is in the /intersection/ of the range r0Range and (1/255, 1)
--   * rf is in the /intersection/ of the range rfRange and (0, r0)
--   * w0 is in the /intersection/ of the range w0Range and (1, 1+s)
--   * wf is in the /intersection/ of the range wfRange and (1/255, 1)
randomDecayingGaussian
  :: RandomGen g
    => RandomDecayingGaussianParams
      -> Rand g (SOM.DecayingGaussian Double)
randomDecayingGaussian p = do
  r0 <- getRandomR $ intersection r0RangeLimits (r0Range p)
  rf <- getRandomR $ intersection (rfRangeLimits r0) (rfRange p)
  w0 <- getRandomR $ intersection (w0RangeLimits $ sideLength p) (w0Range p)
  wf <- getRandomR $ intersection (wfRangeLimits w0) (wfRange p)
  tf <- getRandomR $ intersection (tfRangeLimits) (tfRange p)
  return $ SOM.DecayingGaussian r0 rf w0 wf tf

instance Serialize HexHexGrid where
  put g = S.put (Grid.size g)
  get = do
    n <- S.get
    return $ hexHexGrid n

instance Genetic HexHexGrid where
  put g = G.put . forceIntToWord8 $ Grid.size g
  get = do
    n <- G.get :: G.Reader (Either [String] Word8)
    return $ hexHexGrid <$> fmap fromIntegral n

instance Diploid HexHexGrid where
  express g1 g2 = hexHexGrid $ express (Grid.size g1) (Grid.size g2)

instance (Serialize p) => Serialize (LGridMap HexHexGrid p)

instance (Genetic p) => Genetic (LGridMap HexHexGrid p) where
  put gm = G.put (GM.toGrid gm) >> G.put (GM.elems gm)
  get = do
    g <- G.get
    ps <- G.get
    return $ lazyGridMap <$> g <*> ps
instance (Diploid p) => Diploid (LGridMap HexHexGrid p) where
  express gm1 gm2 = lazyGridMap g ps
    where g = express (GM.toGrid gm1) (GM.toGrid gm2)
          ps = express (GM.elems gm1) (GM.elems gm2)

instance (Serialize f, Serialize t, Serialize p) =>
  Serialize (SOM.SOM f t (LGridMap HexHexGrid) Label p)
instance (Genetic f, Genetic t, Genetic p) =>
  Genetic (SOM.SOM f t (LGridMap HexHexGrid) Label p)
instance (Diploid f, Diploid t, Diploid p) =>
  Diploid (SOM.SOM f t (LGridMap HexHexGrid) Label p)

data GeneticSOM p =
  GeneticSOM
    {
      patternMap :: (SOM.SOM (SOM.DecayingGaussian Double) Word16 (LGridMap HexHexGrid) Label p),
      counterMap :: (LGridMap HexHexGrid Word16)
    }
  deriving (Eq, Show, Generic)

-- | Returns @True@ if the SOM has at least one model; returns @False@
--   otherwise.
somOK
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> Bool
somOK = not . null . models

-- | @'buildGeneticSOM' s f ps@ returns a genetic SOM based on a 
--   hexagonal grid with sides of length @s@, using the learning
--   function @f@, and initialised with the models @ps@.
buildGeneticSOM
  :: (Pattern p, Metric p ~ Double)
    => Word8 -> SOM.DecayingGaussian Double -> [p] -> GeneticSOM p
buildGeneticSOM s f@( SOM.DecayingGaussian r0 _ w0 _ tf) xs
  | null xs   = error "SOM has no models"
  | r0 == 0    = error "r0==0"
  | w0 == 0    = error "w0==0"
  | tf == 0    = error "tf==0"
  | otherwise = GeneticSOM som ks
  where g = hexHexGrid $ fromIntegral s
        gm = lazyGridMap g xs
        som = SOM.SOM gm f 0
        ks = lazyGridMap g (repeat 0)

instance (Serialize p) => Serialize (GeneticSOM p)

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

instance Statistical (GeneticSOM p) where
  stats (GeneticSOM (SOM.SOM gm f _) _) =
    (iStat "IQ" iq):(iStat "edge size" . Grid.size $ gm):(stats f)
    where iq = Grid.tileCount gm

-- randomGeneticSOM
--   :: (Pattern p, Metric p ~ Double, RandomGen g)
--     => Word8 -> [p] -> Rand g (GeneticSOM p)
-- randomGeneticSOM s xs = do
--   r0 <- getRandomR (1/255,1)
--   rf <- getRandomR (0,r0)
--   w0 <- getRandomR (1,1 + fromIntegral s)
--   wf <- getRandomR (1,w0)
--   let tMax = maxBound :: Word16
--   tf <- getRandomR (1,fromIntegral tMax)
--   let f = SOM.DecayingGaussian r0 rf w0 wf tf
--   return $ buildGeneticSOM s f xs

-- | Adjusts the model at the index (grid location) specified.
--   Only the one model is changed. This is useful for allowing wains
--   to learn from each other.
-- TODO: Maybe it would be better to do the normal training instead?
learn
  :: (Pattern p, Metric p ~ Double)
    => p -> Label -> GeneticSOM p -> GeneticSOM p
learn p l s = s { patternMap=gm' }
  where gm = patternMap s
        f = makeSimilar p (currentLearningFunction gm 0)
        gm' = GM.adjust f l gm

-- | Returns the index (grid location) of the model that most closely
--   matches the input pattern. The SOM is not updated.
justClassify
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> p -> Label
justClassify s = C.classify (patternMap s)

-- | Updates the SOM based on the input pattern.
--   Returns the index (grid location) of the model that most closely
--   matches the input pattern,
--   the differences between the input pattern and each model in the
--   SOM,
--   and the updated SOM.
reportAndTrain
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> p -> (Label, [(Label, Metric p)], GeneticSOM p)
reportAndTrain s p = (bmu, diffs, s')
  where (bmu, diffs, som') = C.reportAndTrain (patternMap s) p
        s' = s { patternMap=som', counterMap=GM.adjust (+1) bmu (counterMap s)}

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

-- | Returns a list containing each index (grid location) in the SOM,
--   paired with the model at that index.
toList :: (Pattern p, Metric p ~ Double) => GeneticSOM p -> [(Label, p)]
toList (GeneticSOM s _) = C.toList s

learningFunction :: GeneticSOM p -> SOM.DecayingGaussian Double
learningFunction (GeneticSOM s _) = SOM.learningFunction s

size :: GeneticSOM p -> Int
size (GeneticSOM _ c) = Grid.size c
