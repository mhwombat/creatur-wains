------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.GeneticSOMInternal
-- Copyright   :  (c) Amy de Buitléir 2013-2014
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
{-# LANGUAGE TypeFamilies, DeriveGeneric, FlexibleContexts,
    FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.GeneticSOMInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Wain.ClassificationMetrics (novelty)
import ALife.Creatur.Wain.Statistics (Statistical, iStat, stats)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI,
  uiToDouble)
import ALife.Creatur.Wain.Util (forceIntToWord16, intersection)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Random (Rand, RandomGen, getRandomR)
import Data.Datamining.Pattern (Pattern, Metric)
import qualified Data.Datamining.Clustering.Classifier as C
import qualified Data.Datamining.Clustering.SSOM as SOM
import qualified Data.Map.Strict as M
import qualified Data.Serialize as S
import Data.Word (Word16)
import GHC.Generics (Generic)

type Label = Word16

instance S.Serialize (SOM.Gaussian Double)

instance Genetic (SOM.Gaussian Double) where
  put (SOM.Gaussian r0 rf tf) = do
    G.put $ doubleToUI r0
    G.put $ doubleToUI rf
    G.put . forceIntToWord16 $ round tf
  get = do
    r0 <- G.get :: G.Reader (Either [String] UIDouble)
    rf <- G.get :: G.Reader (Either [String] UIDouble)
    tf <- G.get :: G.Reader (Either [String] Word16)
    return $ SOM.Gaussian <$> fmap uiToDouble r0
      <*> fmap uiToDouble rf <*> fmap fromIntegral tf

instance (Diploid a) => Diploid (SOM.Gaussian a)

data RandomGaussianParams = RandomGaussianParams
  {
    r0Range :: (Double, Double),
    rfRange :: (Double, Double),
    tfRange :: (Double, Double)
  } deriving Show

r0RangeLimits :: (Double, Double)
r0RangeLimits = (1/255, 1)

rfRangeLimits :: Double -> (Double, Double)
rfRangeLimits r0 = (0, r0)

tfRangeLimits :: (Double, Double)
tfRangeLimits = (1,fromIntegral tMax)
  where tMax = maxBound :: Word16

-- | Returns a set of parameters which will permit the broadest possible
--   set of random decaying gaussian functions for a SOM that uses a
--   hexagonal grid of size @s@.
randomGaussianParams :: RandomGaussianParams
randomGaussianParams = 
  RandomGaussianParams
    {
      r0Range = r0RangeLimits,
      rfRange = r0RangeLimits,
      tfRange = tfRangeLimits
    }

-- | @'randomGaussian' r0Range rfRange tMaxRange s@
--   returns a random decaying gaussian that can be used as the
--   learning function for a SOM that uses a hexagonal grid of size @s@.
--   The parameters of the gaussian will be chosen such that:
--
--   * r0 is in the /intersection/ of the range r0Range and (1/255, 1)
--   * rf is in the /intersection/ of the range rfRange and (0, r0)
randomGaussian
  :: RandomGen g
    => RandomGaussianParams
      -> Rand g (SOM.Gaussian Double)
randomGaussian p = do
  r0 <- getRandomR $ intersection r0RangeLimits (r0Range p)
  rf <- getRandomR $ intersection (rfRangeLimits r0) (rfRange p)
  tf <- getRandomR $ intersection (tfRangeLimits) (tfRange p)
  return $ SOM.Gaussian r0 rf tf

validGaussian :: SOM.Gaussian Double -> Bool
validGaussian (SOM.Gaussian r0 rf tf) =
  0 < r0 && r0 <= 1
    && 0 <= rf && rf <= 1 && rf <= r0
    && 0 < tf

-- instance Serialize HexHexGrid where
--   put g = S.put (Grid.size g)
--   get = do
--     n <- S.get
--     return $ hexHexGrid n

-- instance Genetic HexHexGrid where
--   put g = G.put . forceIntToWord8 $ Grid.size g
--   get = do
--     n <- G.get :: G.Reader (Either [String] Word8)
--     return $ hexHexGrid <$> fmap fromIntegral n

-- instance Diploid HexHexGrid where
--   express g1 g2 = hexHexGrid $ express (Grid.size g1) (Grid.size g2)

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
      patternMap :: (SOM.SSOM (SOM.Gaussian Double) Word16 Label p),
      counterMap :: (M.Map Label Word16)
    }
  deriving (Eq, Show, Generic)

-- | Returns @True@ if the SOM has a valid Gaussian and at least one
--   model; returns @False@ otherwise.
somOK
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> Bool
somOK s
  = (not . null . models $ s) && (numModels s > 1)
      && (validGaussian . learningFunction $ s)

-- | @'buildGeneticSOM' s f ps@ returns a genetic SOM based on a 
--   hexagonal grid with sides of length @s@, using the learning
--   function @f@, and initialised with the models @ps@.
buildGeneticSOM
  :: (Pattern p, Metric p ~ Double)
    => SOM.Gaussian Double -> [p] -> GeneticSOM p
buildGeneticSOM f@( SOM.Gaussian r0 _ tf) xs
  | null xs   = error "SOM has no models"
  | r0 == 0    = error "r0==0"
  | tf == 0    = error "tf==0"
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
      :(stats . SOM.learningFunction . patternMap $ s)

-- randomGeneticSOM
--   :: (Pattern p, Metric p ~ Double, RandomGen g)
--     => Word8 -> [p] -> Rand g (GeneticSOM p)
-- randomGeneticSOM s xs = do
--   r0 <- getRandomR (1/255,1)
--   rf <- getRandomR (0,r0)
--   let tMax = maxBound :: Word16
--   tf <- getRandomR (1,fromIntegral tMax)
--   let f = SOM.Gaussian r0 rf tf
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
        gm' = SOM.trainNode gm l p

-- | Returns the index (grid location) of the model that most closely
--   matches the input pattern. The SOM is not updated.
justClassify
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> p -> Label
justClassify s = C.classify (patternMap s)

-- | Updates the SOM based on the input pattern.
--   Returns the index (grid location) of the model that most closely
--   matches the input pattern, the differences between the input
--   pattern and each model in the SOM, the novelty of the input
--   pattern, the adjusted novelty, and the updated SOM (the counter
--   for the closest model is incremented).
reportAndTrain
  :: (Pattern p, Ord (Metric p), Metric p ~ Double)
    => GeneticSOM p -> p
      -> (Label, [(Label, Metric p)], Double, Int, GeneticSOM p)
reportAndTrain s p = (bmu, diffs, nov, adjNov, s')
  where (bmu, diffs, som') = C.reportAndTrain (patternMap s) p
        cMap = M.adjust (+1) bmu (counterMap s)
        s' = s { patternMap=som', counterMap=cMap}
        nov = novelty bmu (M.toList cMap)
        adjNov = round $ nov * (fromIntegral $ SOM.counter som')

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
modelAt s k = (SOM.toMap . patternMap $ s) M.! k

-- | Returns a list containing each index (grid location) in the SOM,
--   paired with the model at that index.
toList :: (Pattern p, Metric p ~ Double) => GeneticSOM p -> [(Label, p)]
toList (GeneticSOM s _) = C.toList s

learningFunction :: GeneticSOM p -> SOM.Gaussian Double
learningFunction (GeneticSOM s _) = SOM.learningFunction s
