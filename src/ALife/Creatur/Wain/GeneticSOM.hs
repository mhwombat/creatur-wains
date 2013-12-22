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
    UndecidableInstances #-}
module ALife.Creatur.Wain.GeneticSOM
  (
    Params(..),
    GeneticSOM(..),
    Label,
    buildGeneticSOM,
    validTimeRange,
    numModels,
    patternCount,
    counts,
    models,
    justClassify,
    reportAndTrain,
    learn,
    randomGeneticSOM
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Util (fromEither)
import ALife.Creatur.Wain.Statistics (Statistical, dStat, iStat, stats)
import ALife.Creatur.Wain.UnitInterval (UIDouble) 
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Random (Rand, RandomGen, getRandom, getRandomR)
import Data.Datamining.Pattern (Pattern, Metric, makeSimilar)
import qualified Data.Datamining.Clustering.Classifier as C
import Data.Datamining.Clustering.SOM (SOM, defaultSOM, toGridMap,
  counter, setCounter)
-- TODO: Either move currentLearningFunction or "learn" to
-- Data.Datamining.Clustering.SOM
import Data.Datamining.Clustering.SOMInternal (currentLearningFunction)
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Word (Word8, Word16)
import GHC.Generics (Generic)
import Math.Geometry.Grid (tileCount)
import Math.Geometry.Grid.Hexagonal (hexHexGrid, HexHexGrid)
import Math.Geometry.GridMap (adjust, elems)
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMap)

validTimeRange :: (Int, Int)
validTimeRange = (0,100000)

type Label = (Int, Int)

data Params = Params
  {
    pSize :: Word8,
    pR0  :: UIDouble,
    pRf  :: UIDouble,
    pW0  :: Word8,
    pWf  :: Word8,
    pTf  :: Word16
  } deriving (Eq, Show, Generic)

instance Statistical (Params) where
  stats (Params s r0 rf w0 wf tf) =
    [ iStat "edge size" . fromIntegral $ s, dStat "r0" r0,
      dStat "rf" rf, iStat "w0" . fromIntegral $ w0,
      iStat "wf" . fromIntegral $ wf, iStat "tf" . fromIntegral $  tf ]

instance Serialize Params
instance Genetic Params
instance Diploid (Params)

randomParams :: RandomGen g => Word8 -> Rand g (Params)
randomParams maxSize = do
  s <- getRandomR (1, maxSize)
  r0 <- getRandomR (0.001,1)
  rf <- getRandomR (0.001,r0)
  w0 <- getRandomR (0,fromIntegral s)
  wf <- getRandomR (0,w0)
  tf <- getRandom
  return $ Params s r0 rf w0 wf tf

data GeneticSOM p =
  GeneticSOM
    {
      sSOM :: SOM (LGridMap HexHexGrid) Label p,
      sParams :: Params,
      sCounters :: LGridMap HexHexGrid Int
    }
  deriving Generic

instance Statistical (GeneticSOM p) where
  stats s = (iStat "IQ" iq):(stats . sParams $ s)
    where iq = tileCount . sSOM $ s

randomGeneticSOM
  :: (Pattern p, Metric p ~ UIDouble, RandomGen g)
    => Word8 -> [p] -> Rand g (GeneticSOM p)
randomGeneticSOM maxSize xs = do
  params <- randomParams maxSize
  return . buildGeneticSOM params $ xs

learn :: Pattern p => p -> Label -> GeneticSOM p -> GeneticSOM p
learn p l s = s { sSOM=gm' }
  where gm = sSOM s
        f = makeSimilar p (currentLearningFunction gm 0)
        gm' = adjust f l gm

justClassify :: (Pattern p, Ord (Metric p)) => GeneticSOM p -> p -> Label
justClassify s = C.classify (sSOM s)

reportAndTrain
  :: (Pattern p, Ord (Metric p))
    => GeneticSOM p -> p -> (Label, [(Label, Metric p)], GeneticSOM p)
reportAndTrain s p = (bmu, diffs, s')
  where (bmu, diffs, som') = C.reportAndTrain (sSOM s) p
        s' = s { sSOM=som', sCounters=adjust (+1) bmu (sCounters s)}
        
numModels :: (Pattern p, Ord (Metric p)) => GeneticSOM p -> Int
numModels (GeneticSOM s _ _) = C.numModels s

models :: (Pattern p, Ord (Metric p)) => GeneticSOM p -> [p]
models (GeneticSOM s _ _) = C.models s

buildGeneticSOM
  :: (Pattern p, Metric p ~ UIDouble)
    => Params -> [p] -> GeneticSOM p
buildGeneticSOM p@(Params s r0 rf w0 wf tf) xs = GeneticSOM som p ks
  where g = hexHexGrid (fromIntegral s)
        gm = lazyGridMap g xs
        som = defaultSOM gm r0 rf (fromIntegral w0) (fromIntegral wf) (fromIntegral tf)
        ks = lazyGridMap g (repeat 0)
        
instance (Eq p, Metric p ~ UIDouble)
      => Eq (GeneticSOM p) where
  (==) (GeneticSOM som1 params1 counts1) (GeneticSOM som2 params2 counts2) = 
      (toGridMap som1 == toGridMap som2) && (counter som1 == counter som2)
        && (params1 == params2) && (counts1 == counts2)

showSOM
  :: (Show p)
    => LGridMap HexHexGrid p -> Params -> Int -> String
showSOM gm (Params _ r0 rf w0 wf tf) k = "setCounter " ++ show k
  ++ " $ defaultSOM (" ++ show gm ++ ") " ++ show r0 ++ " " ++ show rf
  ++ " " ++ show w0 ++ " " ++ show wf ++ " " ++ show tf

instance (Show p, Pattern p, Show (Metric p))
         => Show (GeneticSOM p) where
  show (GeneticSOM som params ns)
    = "GeneticSOM (" ++ showSOM gm params k ++ ") (" ++ show params ++ ") ("
        ++ show ns ++ ")"
    where gm = toGridMap som
          k = counter som
  -- show (GeneticSOM som params ns)
  --   = "(\\d -> d { sSOM=setCounter " ++ show (counter som)
  --     ++ " (sSOM d) }) (buildGeneticSOM (" ++ show params ++ ") "
  --     ++ show (C.models som) ++ show ns ++ ")"

instance (Pattern p, Serialize [p], Metric p ~ UIDouble)
         => Serialize (GeneticSOM p) where
  put d = S.put (models d)
          >> S.put (counter . sSOM $ d)
          >> S.put (sParams d)
  get = do
    xs <- S.get
    k <- S.get
    params <- S.get
    let d = buildGeneticSOM params xs
    let som = setCounter k . sSOM $ d
    return $ d { sSOM = som }

instance (Genetic p, Pattern p, Metric p ~ UIDouble)
  => Genetic (GeneticSOM p) where
  put d = G.put (models d) >> G.put (sParams d)
  get = do
    xs <- G.get
    if null (fromEither [] xs)
      then return $ Left ["SOM has no initial models"]
      else do
        params <- G.get
        case params of
          Right p -> if pSize p == 0
                     then return $ Left ["Null SOM"]
                     else return $ buildGeneticSOM <$> params <*> xs
          Left e -> return $ Left e

instance (Diploid p, Pattern p, Metric p ~ UIDouble)
  => Diploid (GeneticSOM p) where
  express (GeneticSOM s1 p1 _) (GeneticSOM s2 p2 _) =
    buildGeneticSOM (express p1 p2) (express (C.models s1) (C.models s2))

patternCount :: GeneticSOM p -> Int
patternCount = sum . elems . sCounters

counts :: GeneticSOM p -> [Int]
counts = elems . sCounters
