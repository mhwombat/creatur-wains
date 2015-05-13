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
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.GeneticSOMInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Wain.Statistics (Statistical, iStat, uiStat, stats)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI,
  uiToDouble)
import ALife.Creatur.Wain.Util (intersection)
import Control.Lens
import Control.Monad.Random (Rand, RandomGen, getRandomR)
import qualified Data.Datamining.Clustering.Classifier as C
import qualified Data.Datamining.Clustering.SSOM as SOM
import qualified Data.Map.Strict as M
import qualified Data.Serialize as S
import Data.Word (Word16)
import GHC.Generics (Generic)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif

type Label = Word16

data ExponentialParams = ExponentialParams Double Double
  deriving (Eq, Show, Generic)

instance S.Serialize ExponentialParams

instance Genetic ExponentialParams where
  put (ExponentialParams r0 d) = do
    G.put $ doubleToUI r0
    G.put $ doubleToUI d
  get = do
    r0 <- G.get :: G.Reader (Either [String] UIDouble)
    d <- G.get :: G.Reader (Either [String] UIDouble)
    return $ ExponentialParams <$> fmap uiToDouble r0
      <*> fmap uiToDouble d

instance Diploid ExponentialParams

instance Statistical ExponentialParams where
  stats (ExponentialParams r0 d)
    = [uiStat "r0" r0, uiStat "d" d]

validExponential :: ExponentialParams -> Bool
validExponential (ExponentialParams r0 d) = 0 <= r0 && r0 <= 1
                                              && 0 <= d && d <= 1

toExponential :: ExponentialParams -> Word16 -> Double
toExponential (ExponentialParams r0 d)
  = SOM.exponential r0 d . fromIntegral

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
      -> Rand g ExponentialParams
randomExponential p = do
  r0 <- getRandomR . intersection r0RangeLimits . _r0Range $ p
  d <- getRandomR . intersection dRangeLimits . _dRange $ p
  return $ ExponentialParams r0 d

instance (Genetic k, Ord k, Genetic p) => Genetic (M.Map k p) where
  put gm = G.put (M.toList gm)
  get = do
    xs <- G.get
    return $ M.fromList <$> xs
instance (Ord k, Diploid p) => Diploid (M.Map k p) where
  express gm1 gm2 = M.fromList . zip ks $ vs
    where ks = M.keys gm1
          vs = express (M.elems gm1) (M.elems gm2)

class Thinker t where
  type Pattern t
  -- | Compares two patterns and returns a /non-negative/ number
  --   representing how different the patterns are.
  --   The difference should be between @0@ and @1@, inclusive.
  --   A result of @0@ indicates that the patterns are identical.
  diff :: t -> Pattern t -> Pattern t -> Double
  -- | @'makeSimilar' f target r pattern@ returns a modified copy
  --   of @pattern@ that is more similar to @target@ than @pattern@ is.
  --   The magnitude of the adjustment is controlled by the @r@
  --   parameter, which should be a number between 0 and 1. Larger
  --   values for @r@ permit greater adjustments. If @r@=1,
  --   the result should be identical to the @target@. If @r@=0,
  --   the result should be the unmodified @pattern@.
  adjust :: t -> Pattern t -> Double -> Pattern t -> Pattern t

data GeneticSOM p t =
  GeneticSOM
    {
      _patternMap :: SOM.SSOM Word16 Double Label p,
      _counterMap :: M.Map Label Word16,
      _exponentialParams :: ExponentialParams,
      _teacher :: t
    }
makeLenses ''GeneticSOM

instance (Eq p, Eq t) => Eq (GeneticSOM p t) where
  (==) x y = (SOM.sMap . _patternMap $ x) == (SOM.sMap . _patternMap $ y)
             && (SOM.counter . _patternMap $ x) == (SOM.counter . _patternMap $ y)
             && _exponentialParams x == _exponentialParams y
             && _teacher x == _teacher y

instance (Show p, Show t) => Show (GeneticSOM p t) where
  show x = "GeneticSOM (" ++ showSSOM s e t
             ++ ") (" ++ show c
             ++ ") (" ++ show e
             ++ ") (" ++ show t ++ ")"
    where s = _patternMap x
          c = _counterMap x
          e = _exponentialParams x
          t = _teacher x

showSSOM
  :: (Show p, Show t)
    => SOM.SSOM Word16 Double Label p -> ExponentialParams -> t -> String
showSSOM s e t = "SSOM (" ++ show (SOM.sMap s)
                   ++ ") (toExponential (" ++ show e
                   ++ ")) (diff (" ++ show t
                   ++ ")) (adjust (" ++ show t
                   ++ ")) " ++ show (SOM.counter s)

instance (Diploid p, Diploid t, Thinker t, p ~ Pattern t)
    => Diploid (GeneticSOM p t) where
  express x y = buildGeneticSOM e t xs
    where xs = express (M.elems . SOM.sMap . _patternMap $ x) (M.elems . SOM.sMap . _patternMap $ y)
          e = express (_exponentialParams x) (_exponentialParams y)
          t = express (_teacher x) (_teacher y)
  
instance (S.Serialize p, S.Serialize t, Thinker t, p ~ Pattern t)
    => S.Serialize (GeneticSOM p t) where
  put s = S.put (SOM.toMap . _patternMap $ s)
            >> S.put (SOM.counter . _patternMap $ s)
            >> S.put (_counterMap s)
            >> S.put (_exponentialParams s)
            >> S.put (_teacher s)
  get = do
    gm <- S.get
    t <- S.get
    cm <- S.get
    eps <- S.get
    tr <- S.get
    let lrf = toExponential eps
    let df = diff tr
    let af = adjust tr
    let s = SOM.SSOM gm lrf df af t
    return $ GeneticSOM s cm eps tr

instance (G.Genetic p, G.Genetic t, Thinker t, p ~ Pattern t)
    => G.Genetic (GeneticSOM p t) where
  put s = G.put (SOM.toMap . _patternMap $ s)
            >> G.put (SOM.counter . _patternMap $ s)
            >> G.put (_counterMap s)
            >> G.put (_exponentialParams s)
            >> G.put (_teacher s)
  get = do
    gm <- safeGetPatternMap
    t <- G.get
    cm <- G.get
    eps <- G.get
    let lrf = fmap toExponential eps
    tr <- G.get
    let df = fmap diff tr
    let af = fmap adjust tr
    let s = SOM.SSOM <$> gm <*> lrf <*> df <*> af <*> t
    return $ GeneticSOM <$> s <*> cm <*> eps <*> tr

safeGetPatternMap
  :: Genetic p
    => G.Reader (Either [String] (M.Map Label p)) 
safeGetPatternMap = do
  x <- G.get
  case x of
    Right gm  -> if null . M.elems $ gm
                  then return $ Left ["Genetic SOM has no models"]
                  else return $ Right gm
    Left msg -> return $ Left msg

-- instance (Diploid p) => Diploid (GeneticSOM p t) where
--   express 


-- | Returns @True@ if the SOM has a valid Exponential and at least one
--   model; returns @False@ otherwise.
somOK
  :: GeneticSOM p t -> Bool
somOK s
  = (not . null . models $ s) && (numModels s > 1)
      && (validExponential . _exponentialParams $ s)

-- | @'buildGeneticSOM' f ps@ returns a genetic SOM, using the learning
--   function @f@, and initialised with the models @ps@.
buildGeneticSOM
  :: (Thinker t, p ~ Pattern t)
    => ExponentialParams -> t -> [p] -> GeneticSOM p t
buildGeneticSOM e@(ExponentialParams r0 d) t xs
  | null xs   = error "SOM has no models"
  | r0 < 0    = error "r0<0"
  | d < 0     = error "d<0"
  | otherwise = GeneticSOM som ks e t
  where gm = M.fromList . zip [0..] $ xs
        zeros = map (const 0) xs
        som = SOM.SSOM gm lrf df af 0
        lrf = toExponential e
        df = diff t
        af = adjust t
        ks = M.fromList . zip [0..] $ zeros

instance Statistical (GeneticSOM p t) where
  stats s =
    (iStat "num models" . numModels $ s)
      :(iStat "SQ" . schemaQuality $ s)
      :(stats . _exponentialParams $ s)

-- | Adjusts the model at the index (grid location) specified.
--   Only the one model is changed. This is useful for allowing wains
--   to learn from each other.
-- TODO: Maybe it would be better to do the normal training instead?
learn
  :: p -> Label -> GeneticSOM p t -> GeneticSOM p t
learn p l s = set patternMap gm' s
  where gm = _patternMap s
        gm' = SOM.trainNode gm l p

-- | Returns the index (grid location) of the model that most closely
--   matches the input pattern. The SOM is not updated.
justClassify
  :: GeneticSOM p t -> p -> Label
justClassify s = C.classify (_patternMap s)

-- | Updates the SOM based on the input pattern.
--   Returns the index (grid location) of the model that most closely
--   matches the input pattern, the differences between the input
--   pattern and each model in the SOM, the novelty of the input
--   pattern, and the updated SOM (the counter for the closest model
--   is incremented).
reportAndTrain
  :: GeneticSOM p t -> p
      -> (Label, [(Label, Double)], Double, GeneticSOM p t)
reportAndTrain s p = (bmu, diffs', novelty, s')
  where (bmu, diffs, som') = C.reportAndTrain (_patternMap s) p
        diffs' = validateDiffs diffs
        cMap = M.adjust (+1) bmu (_counterMap s)
        s' = set patternMap som' . set counterMap cMap $ s
        novelty = minimum . map snd $ diffs

validateDiffs :: [(Label, Double)] -> [(Label, Double)]
validateDiffs = map f
  where f (l, x) = if 0 <= x && x <= 1
                     then (l, x)
                     else (error "Difference is out of range")

-- | Returns the number of models in the SOM.
numModels
  :: GeneticSOM p t -> Int
numModels (GeneticSOM s _ _ _) = C.numModels s

-- | Returns the models in the SOM.
models
  :: GeneticSOM p t -> [p]
models (GeneticSOM s _ _ _) = C.models s

modelAt :: GeneticSOM p t -> Label -> p
modelAt s k = SOM.toMap (_patternMap s) M.! k

-- | Returns a list containing each index (grid location) in the SOM,
--   paired with the model at that index.
toList :: GeneticSOM p t -> [(Label, p)]
toList (GeneticSOM s _ _ _) = C.toList s

currentLearningRate :: GeneticSOM p t -> Double
currentLearningRate s
  = (SOM.learningRate . _patternMap $ s) (fromIntegral t)
  where t = SOM.counter . _patternMap $ s

schemaQuality :: GeneticSOM p t -> Int
schemaQuality = discrimination . M.elems . _counterMap

discrimination :: Integral a => [a] -> Int
discrimination xs = length $ filter (>k) xs
  where k = sum xs `div` fromIntegral (2 * length xs)
