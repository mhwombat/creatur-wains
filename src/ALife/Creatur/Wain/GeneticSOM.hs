------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.GeneticSOM
-- Copyright   :  (c) Amy de Buitléir 2013
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
    validTimeRange,
    numModels,
    patternCount,
    setCounts,
    counts,
    counterList,
    models,
    justClassify,
    reportAndTrain,
    learn,
    randomGeneticSOM,
    somOK
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Wain.Statistics (Statistical, iStat, stats)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI,
  uiToDouble)
import ALife.Creatur.Wain.Util (forceIntToWord8, forceIntToWord16)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Random (Rand, RandomGen, getRandom, getRandomR)
import Data.Datamining.Pattern (Pattern, Metric, makeSimilar)
import qualified Data.Datamining.Clustering.Classifier as C
import Data.Datamining.Clustering.SOM (SOM(..), DecayingGaussian(..))
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

validTimeRange :: (Int, Int)
validTimeRange = (0,100000)

type Label = (Int, Int)

instance Serialize (DecayingGaussian Double)

instance Genetic (DecayingGaussian Double) where
  put (DecayingGaussian r0 rf w0 wf tf) = do
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
    return $ DecayingGaussian <$> fmap uiToDouble r0
      <*> fmap uiToDouble rf <*> fmap fromIntegral w0
      <*> fmap fromIntegral wf <*> fmap fromIntegral tf

instance (Diploid a) => Diploid (DecayingGaussian a)

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
  Serialize (SOM f t (LGridMap HexHexGrid) Label p)
instance (Genetic f, Genetic t, Genetic p) =>
  Genetic (SOM f t (LGridMap HexHexGrid) Label p)
instance (Diploid f, Diploid t, Diploid p) =>
  Diploid (SOM f t (LGridMap HexHexGrid) Label p)

data GeneticSOM p =
  GeneticSOM
    {
      sSOM :: (SOM (DecayingGaussian Double) Word16 (LGridMap HexHexGrid) Label p),
      sCounters :: (LGridMap HexHexGrid Word16)
    }
  deriving (Eq, Show, Generic)

somOK
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> Bool
somOK = not . null . models

buildGeneticSOM
  :: (Pattern p, Metric p ~ Double)
    => Word8 -> DecayingGaussian Double -> [p] -> GeneticSOM p
buildGeneticSOM s f xs =
  if null xs
    then error "SOM has no models"
    else GeneticSOM som ks
  where g = hexHexGrid $ fromIntegral s
        gm = lazyGridMap g xs
        som = SOM gm f 0
        ks = lazyGridMap g (repeat 0)

instance (Serialize p) => Serialize (GeneticSOM p)

instance (Genetic p) => Genetic (GeneticSOM p)
-- instance (Genetic p) => Genetic (GeneticSOM p) where
--   put = G.put . sSOM
--   get = do
--     som <- G.get
--     case som of
--       Right s ->
--         if (Grid.null . gridMap $ sSOM s)
--           then return $ Left ["Genetic SOM has no models"]
--           else return $ Right s
--       Left ms -> return $ Left ms

instance (Diploid p) => Diploid (GeneticSOM p)

instance Statistical (GeneticSOM p) where
  stats (GeneticSOM (SOM gm f _) _) =
    (iStat "IQ" iq):(iStat "edge size" . Grid.size $ gm):(stats f)
    where iq = Grid.tileCount gm

randomGeneticSOM
  :: (Pattern p, Metric p ~ Double, RandomGen g)
    => Word8 -> [p] -> Rand g (GeneticSOM p)
randomGeneticSOM s xs = do
  r0 <- getRandomR (0.001,1)
  rf <- getRandomR (0.001,r0)
  w0 <- getRandomR (0,fromIntegral s)
  wf <- getRandomR (0,w0)
  tf <- getRandom
  let f = DecayingGaussian r0 rf w0 wf tf
  return $ buildGeneticSOM s f xs

learn
  :: (Pattern p, Metric p ~ Double)
    => p -> Label -> GeneticSOM p -> GeneticSOM p
learn p l s = s { sSOM=gm' }
  where gm = sSOM s
        f = makeSimilar p (currentLearningFunction gm 0)
        gm' = GM.adjust f l gm

justClassify
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> p -> Label
justClassify s = C.classify (sSOM s)

reportAndTrain
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> p -> (Label, [(Label, Metric p)], GeneticSOM p)
reportAndTrain s p = (bmu, diffs, s')
  where (bmu, diffs, som') = C.reportAndTrain (sSOM s) p
        s' = s { sSOM=som', sCounters=GM.adjust (+1) bmu (sCounters s)}
        
numModels
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> Int
numModels (GeneticSOM s _) = C.numModels s

models
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> [p]
models (GeneticSOM s _) = C.models s

setCounts
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => [Word16] -> GeneticSOM p -> GeneticSOM p
setCounts ks (GeneticSOM s kMap) = GeneticSOM s' kMap'
  where kMap' = lazyGridMap (GM.toGrid kMap) ks
        s' = s { counter=(sum ks) }

patternCount :: Metric p ~ Double => GeneticSOM p -> Word16
patternCount = sum . GM.elems . sCounters

counts :: Metric p ~ Double => GeneticSOM p -> [Word16]
counts = GM.elems . sCounters

counterList :: Metric p ~ Double => GeneticSOM p -> [(Label, Word16)]
counterList = GM.toList . sCounters
