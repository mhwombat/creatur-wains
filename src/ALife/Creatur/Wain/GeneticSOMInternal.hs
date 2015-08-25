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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.GeneticSOMInternal where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Wain.Statistics (Statistical, iStat, dStat, stats)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI,
  uiToDouble)
import ALife.Creatur.Wain.Util (intersection)
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad.Random (Rand, RandomGen, getRandomR)
import qualified Data.Datamining.Clustering.SOS as SOM
import qualified Data.Map.Strict as M
import qualified Data.Serialize as S
import Data.Word (Word16)
import GHC.Generics (Generic)

-- | A unique identifier for a model in a SOM.
type Label = Word16

-- | A measure of the difference between an input pattern and a model.
type Difference = UIDouble

-- | @'ExponentialParams' r0 d@ defines the first two parameters for
--     an exponential learning function.
--   See @'Data.Datamining.Clustering.SOS.exponential'@ for more
--   information.
data ExponentialParams = ExponentialParams UIDouble UIDouble
  deriving (Eq, Show, Generic, NFData)

instance S.Serialize ExponentialParams
instance Genetic ExponentialParams
instance Diploid ExponentialParams

instance Statistical ExponentialParams where
  stats (ExponentialParams r0 d)
    = [dStat "r0" (uiToDouble r0), dStat "d" (uiToDouble d)]

-- | Returns true if the parameters for an exponential function are
--   valid, false otherwise.
validExponential :: ExponentialParams -> Bool
validExponential (ExponentialParams r0 d) = 0 <= r0 && r0 <= 1
                                              && 0 <= d && d <= 1

-- @'toExponential' p t@ returns the learning rate at time @t@,
-- given an exponential learning function with parameters @p@.
toExponential :: ExponentialParams -> Word16 -> UIDouble
toExponential (ExponentialParams r0 d)
  = doubleToUI . SOM.exponential (uiToDouble r0) (uiToDouble d)

data RandomExponentialParams = RandomExponentialParams
  {
    _r0Range :: (UIDouble, UIDouble),
    _dRange :: (UIDouble, UIDouble)
  } deriving Show
makeLenses ''RandomExponentialParams

r0RangeLimits :: (UIDouble, UIDouble)
r0RangeLimits = (doubleToUI (1/65535), doubleToUI 1)

dRangeLimits :: (UIDouble, UIDouble)
dRangeLimits = (doubleToUI (1/65535), doubleToUI 1)

-- | Returns a set of parameters which will permit the broadest possible
--   set of random decaying gaussian functions for a SOM.
randomExponentialParams :: RandomExponentialParams
randomExponentialParams =
  RandomExponentialParams r0RangeLimits dRangeLimits

-- | @'randomExponential' ('RandomExponentialParams' r0Range dRange)@
--   returns a random decaying
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

instance (Genetic k, Ord k, Genetic p)
    => Genetic (M.Map k p) where
  put gm = G.put (M.toList gm)
  get = do
    xs <- G.get
    return $ M.fromList <$> xs

instance (Ord k, Diploid p) => Diploid (M.Map k p) where
  express gm1 gm2 = M.fromList . zip ks $ vs
    where ks = M.keys gm1
          vs = express (M.elems gm1) (M.elems gm2)

-- | A "tweaker" is responsible for comparing and adjusting patterns.
class Tweaker t where
  type Pattern t
  -- | Compares two patterns and returns a /non-negative/ number
  --   representing how different the patterns are.
  --   The difference should be between @0@ and @1@, inclusive.
  --   A result of @0@ indicates that the patterns are identical.
  diff :: t -> Pattern t -> Pattern t -> Difference
  -- | @'adjust' t target r pattern@ returns a modified copy
  --   of @pattern@ that is more similar to @target@ than @pattern@ is.
  --   The magnitude of the adjustment is controlled by the @r@
  --   parameter, which should be a number between 0 and 1. Larger
  --   values for @r@ permit greater adjustments. If @r@=1,
  --   the result should be identical to the @target@. If @r@=0,
  --   the result should be the unmodified @pattern@.
  adjust :: t -> Pattern t -> UIDouble -> Pattern t -> Pattern t

-- | A Simplified Self-Organising Map (SOM).
--   @p@ is the type of the input patterns and models.
--   @t@ is the type of the tweaker.
data GeneticSOM p t =
  GeneticSOM
    {
      _patternMap :: SOM.SOS Word16 UIDouble Label p,
      _exponentialParams :: ExponentialParams,
      _tweaker :: t
    } deriving (Generic, NFData)
makeLenses ''GeneticSOM

-- | @'buildGeneticSOM' e n dt t@ returns a genetic SOM, using an
--   exponential function defined by the parameters @e@ as a learning
--   function, maximum number of models @n@, difference threshold @dt@,
--   and "tweaker" @t@.
buildGeneticSOM
  :: (Tweaker t, p ~ Pattern t)
    => ExponentialParams -> Word16 -> UIDouble -> t -> GeneticSOM p t
buildGeneticSOM e@(ExponentialParams r0 d) maxSz dt t
  | r0 < 0    = error "r0<0"
  | d < 0     = error "d<0"
  | otherwise = GeneticSOM som e t
  where som = SOM.makeSOS lrf (fromIntegral maxSz) dt False df af
        lrf = toExponential e
        df = diff t
        af = adjust t

instance (Eq p, Eq t) => Eq (GeneticSOM p t) where
  (==) x y = (SOM.toMap . _patternMap $ x) == (SOM.toMap . _patternMap $ y)
             && _exponentialParams x == _exponentialParams y
             && _tweaker x == _tweaker y

instance (Show p, Show t) => Show (GeneticSOM p t) where
  show x = "GeneticSOM (" ++ showSOS s e t
             ++ ") (" ++ show e
             ++ ") (" ++ show t ++ ")"
    where s = _patternMap x
          e = _exponentialParams x
          t = _tweaker x

-- | Formats a genetic SOM for display.
showSOS
  :: (Show p, Show t)
    => SOM.SOS Word16 UIDouble Label p -> ExponentialParams -> t -> String
showSOS s e t = "SOS (" ++ show (SOM.toMap s)
                   ++ ") (toExponential (" ++ show e
                   ++ ")) " ++ show (SOM.maxSize s)
                   ++ " " ++ show (SOM.diffThreshold s)
                   ++ " " ++ show (SOM.allowDeletion s)
                   ++ " (diff (" ++ show t
                   ++ ")) (adjust (" ++ show t
                   ++ ")) " ++ show (SOM.nextIndex s)

instance (S.Serialize p, S.Serialize t, Tweaker t, p ~ Pattern t)
    => S.Serialize (GeneticSOM p t) where
  put s = S.put (SOM.toMap . _patternMap $ s)
            >> S.put (_exponentialParams s)
            >> S.put (SOM.maxSize . _patternMap $ s)
            >> S.put (SOM.diffThreshold . _patternMap $ s)
            >> S.put (_tweaker s)
            >> S.put (SOM.nextIndex . _patternMap $ s)
  get = do
    gm <- S.get
    eps <- S.get
    maxSz <- S.get
    dt <- S.get
    tr <- S.get
    kNext <- S.get
    let lrf = toExponential eps
    let df = diff tr
    let af = adjust tr
    let s = SOM.SOS gm lrf maxSz dt False df af kNext
    return $ GeneticSOM s eps tr

instance (G.Genetic p, G.Genetic t, Tweaker t, p ~ Pattern t)
    => G.Genetic (GeneticSOM p t) where
  put s = G.put (M.elems . SOM.toMap . _patternMap $ s)
            >> G.put (_exponentialParams s)
            >> G.put (fromIntegral . SOM.maxSize . _patternMap $ s :: Word16)
            >> G.put (SOM.diffThreshold . _patternMap $ s)
            >> G.put (_tweaker s)
  get = do
    nodes <- G.get
    let newLabels = [minBound..] :: [Word16]
    let gm = M.fromList . zip newLabels <$> nodes
    eps <- G.get
    maxSz <- fmap fromIntegral <$> (G.get :: G.Reader (Either [String] Word16))
    dt <- G.get
    tr <- G.get
    let lrf = toExponential <$> eps
    let df = diff <$> tr
    let af = adjust <$> tr
    let kNext = toEnum . length <$> nodes
    let s = SOM.SOS <$> gm <*> lrf <*> maxSz <*> dt <*> pure False <*> df <*> af <*> kNext
    return $ GeneticSOM <$> s <*> eps <*> tr

instance (Diploid p, Diploid t, Tweaker t, p ~ Pattern t)
    => Diploid (GeneticSOM p t) where
  express x y = GeneticSOM s eps tr
    where s = SOM.SOS gm lrf maxSz dt False df af kNext
          gm = M.fromList $
                 express (M.toList . SOM.toMap . _patternMap $ x)
                         (M.toList . SOM.toMap . _patternMap $ y)
          lrf = toExponential eps
          maxSz = express (SOM.maxSize . _patternMap $ x)
                          (SOM.maxSize . _patternMap $ y)
          dt = express (SOM.diffThreshold . _patternMap $ x)
                       (SOM.diffThreshold . _patternMap $ y)
          df = diff tr
          af = adjust tr
          kNext = express (SOM.nextIndex . _patternMap $ x)
                          (SOM.nextIndex . _patternMap $ y)
          eps = express (_exponentialParams x) (_exponentialParams y)
          tr = express (_tweaker x) (_tweaker y)

-- | Returns @True@ if the SOM has a valid Exponential;
--   returns @False@ otherwise.
somOK
  :: GeneticSOM p t -> Bool
somOK = validExponential . _exponentialParams

instance Statistical (GeneticSOM p t) where
  stats s =
    (iStat "num models" . numModels $ s)
      :(iStat "max. size" . SOM.maxSize . _patternMap $ s)
      :(dStat "threshold" . SOM.diffThreshold . _patternMap $ s)
      :(iStat "SQ" . schemaQuality $ s)
      :(stats . _exponentialParams $ s)

-- -- | @'learn' p l s@ adjusts the model at the index (grid location) @l@
-- --   in @s@ to more closely match the pattern @p@.
-- --   The amount of adjusted is determined by the current learning rate
-- --   of the SOM @s@.
-- --   Only the one model is changed. This is useful for allowing wains
-- --   to learn from each other.
-- --   The difference between @'learn'@ and @'train'@ is that when using
-- --   @'learn'@, you specify which model should be adjusted,
-- --   whereas with @'train'@, the closest matching model is adjusted.
-- learn
--   :: p -> Label -> GeneticSOM p t -> GeneticSOM p t
-- learn p l s = set patternMap gm' s
--   where gm = _patternMap s
--         gm' = SOM.trainNode gm l p

-- | Returns the number of models in the SOM.
numModels
  :: GeneticSOM p t -> Int
numModels (GeneticSOM s _ _) = SOM.numModels s

-- -- | Returns the models in the SOM.
-- models
--   :: GeneticSOM p t -> [p]
-- models (GeneticSOM s _ _) = C.models s

-- -- | Returns the model at the specified location in the SOM.
-- modelAt :: GeneticSOM p t -> Label -> p
-- modelAt s k = SOM.toMap (_patternMap s) M.! k

-- -- | Returns a list containing each index (grid location) in the SOM,
-- --   paired with the model at that index.
-- toList :: GeneticSOM p t -> [(Label, p)]
-- toList (GeneticSOM s _ _) = C.toList s

-- | Returns the SOM's current learning rate.
currentLearningRate :: GeneticSOM p t -> UIDouble
currentLearningRate s
  = (SOM.learningRate . _patternMap $ s) (fromIntegral t)
  where t = SOM.time . _patternMap $ s

-- | Measures the quality of the classification system represented by
--   the SOM, and returns the result.
schemaQuality :: GeneticSOM p t -> Int
schemaQuality = discrimination . M.elems . SOM.counterMap . _patternMap

discrimination :: Integral a => [a] -> Int
discrimination xs = length $ filter (>k) xs
  where k = sum xs `div` fromIntegral (2 * length xs)

-- | Returns true if the SOM has no models, false otherwise.
isEmpty :: GeneticSOM p t -> Bool
isEmpty = SOM.isEmpty . _patternMap

-- | The maximum number of models this SOM can hold.
maxSize :: GeneticSOM p t -> Int
maxSize = SOM.maxSize . _patternMap

-- | Returns a map from node ID to model.
modelMap :: GeneticSOM p t -> M.Map Label p
modelMap = SOM.modelMap . _patternMap

-- | Returns a map from node label to counter (number of times the
--   node's model has been the closest match to an input pattern).
counterMap :: GeneticSOM p t -> M.Map Label Word16
counterMap = SOM.counterMap . _patternMap

-- | @'classify' s p@ identifies the model @s@ that most closely
--   matches the pattern @p@.
--   If necessary, it will create a new node and model.
--   Returns the ID of the node with the best matching model,
--   the "novelty" of the the pattern @p@ (difference between it and
--   the best matching model),
--   the differences between the input and each model in the SOM,
--   and the (possibly updated) SOM.
classify
  :: GeneticSOM p t -> p
    -> (Label, Difference, [(Label, Difference)], GeneticSOM p t)
classify gs p = (bmu, bmuDiff, diffs, gs')
  where (bmu, bmuDiff, diffs, s') = SOM.classify s p
        s = view patternMap gs
        gs' = set patternMap s' gs

-- | @'train' s p@ identifies the model in @s@ that most closely
--   matches @p@, and updates it to be a somewhat better match.
train :: GeneticSOM p t -> p -> GeneticSOM p t
train gs p = set patternMap s' gs
  where s' = SOM.train s p
        s = view patternMap gs
