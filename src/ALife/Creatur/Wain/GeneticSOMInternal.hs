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
import ALife.Creatur.Wain.Util (intersection, inRange)
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad.Random (Rand, RandomGen, getRandomR)
import qualified Data.Datamining.Clustering.SGMInternal as SOM
import qualified Data.Map.Strict as M
import qualified Data.Serialize as S
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | A unique identifier for a model in a SOM.
type Label = Word64

-- | A measure of the difference between an input pattern and a model.
type Difference = UIDouble

-- | Private constructor.
--   @'LearningParams' r0 d@ defines the first two parameters for
--     a learning function.
--   When t = 0, the learning rate is r0.
--   Over time the learning rate decays,
--   so that when t = tf, the learning rate is rf.
--   Normally the parameters are chosen such that:
--     0 < r0 <= 1
--     0 < rf <= r0
data LearningParams = LearningParams UIDouble UIDouble Word64
  deriving (Eq, Show, Generic, NFData)

-- | @'mkLearningParams' r0 d@ defines the first two parameters for
--     a learning function.
--   When t = 0, the learning rate is r0.
--   Over time the learning rate decays,
--   so that when t = tf, the learning rate is rf.
--   Normally the parameters are chosen such that:
--     0 < r0 <= 1
--     0 < rf <= r0
mkLearningParams :: UIDouble -> UIDouble -> Word64 -> LearningParams
mkLearningParams r0 rf tf
  | r0 == 0   = error "r0 must be > 0"
  | rf > r0   = error "rf must be <= r0"
  | otherwise = LearningParams r0 rf tf

instance S.Serialize LearningParams
instance Genetic LearningParams
instance Diploid LearningParams

instance Statistical LearningParams where
  stats (LearningParams r0 rf tf)
    = [dStat "r0" (uiToDouble r0), dStat "rf" (uiToDouble rf),
       iStat "tf" tf]

-- | @'toLearningFunction' p t@ returns the learning rate at time @t@,
--   given an exponential learning function with parameters @p@.
toLearningFunction :: LearningParams -> Word64 -> UIDouble
toLearningFunction (LearningParams r0 rf tf) t
  | inRange (0,1) r = doubleToUI r
  | otherwise       = error $ "toLearningFunction: out of bounds"
                                ++ " r0=" ++ show r0
                                ++ " rf=" ++ show rf
                                ++ " tf=" ++ show tf
                                ++ " t=" ++ show t
                                ++ " r=" ++ show r
  where r = r0' * ((rf'/r0')**a)
        a = fromIntegral t / fromIntegral tf
        r0' = uiToDouble r0
        rf' = uiToDouble rf

-- | A set of parameters to constrain the result when generating
--   random learning functions.
data RandomLearningParams = RandomLearningParams
  {
    -- | The range from which the initial learning rate (at t=0)
    --   should be chosen.
    _r0Range :: (UIDouble, UIDouble),
    -- | The range from which the final learning rate (at t=@tf@)
    --   should be chosen.
    _rfRange :: (UIDouble, UIDouble),
    -- | The range from which the final time should be chosen.
    _tfRange :: (Word64, Word64)
  } deriving Show
makeLenses ''RandomLearningParams

-- | Range of values permitted for @r0@
r0RangeLimits :: (UIDouble, UIDouble)
r0RangeLimits = (doubleToUI (1/65535), 1)

-- | Range of values permitted for @rf@
rfRangeLimits :: (UIDouble, UIDouble)
rfRangeLimits = (doubleToUI (1/65535), 1)

-- | Range of values permitted for @rf@
tfRangeLimits :: (Word64, Word64)
tfRangeLimits = (1, maxBound)

-- | Returns a set of parameters which will permit the broadest possible
--   set of random decaying gaussian functions for a SOM.
randomLearningParams :: RandomLearningParams
randomLearningParams =
  RandomLearningParams r0RangeLimits rfRangeLimits tfRangeLimits

-- | @'randomLearningFunction' ('RandomLearningParams' r0Range dRange)@
--   returns a random decaying function that can be used as the
--   learning function for an SGM.
--   The parameters of the gaussian will be chosen such that:
--
--   * r0 is in r0Range, but also 0 < r0 <= 1
--   * rf is in rfRange, but also 0 < rf <= 1
--   * tf is in tfRange, but also 0 < tf
randomLearningFunction
  :: RandomGen g
    => RandomLearningParams
      -> Rand g LearningParams
randomLearningFunction p = do
  r0 <- getRandomR . intersection r0RangeLimits . _r0Range $ p
  rf <- getRandomR . intersection (0, r0) . intersection rfRangeLimits . _rfRange $ p
  tf <- getRandomR . intersection tfRangeLimits . _tfRange $ p
  return $ mkLearningParams r0 rf tf

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

-- | A "tweaker" is responsible for comparing and adjusting models.
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
      -- | The models
      _patternMap :: SOM.SGM Word64 UIDouble Label p,
      -- | The parameters that define the learning function
      _learningParams :: LearningParams,
      -- | The object responsible for comparing and adjusting models.
      _tweaker :: t
    } deriving (Generic, NFData)
makeLenses ''GeneticSOM

-- | @'buildGeneticSOM' e n dt t@ returns a genetic SOM, using an
--   exponential function defined by the parameters @e@ as a learning
--   function, maximum number of models @n@, difference threshold @dt@,
--   and "tweaker" @t@.
buildGeneticSOM
  :: (Tweaker t, p ~ Pattern t)
    => LearningParams -> Word64 -> UIDouble -> t -> GeneticSOM p t
buildGeneticSOM e maxSz dt t
  = GeneticSOM som e t
  where som = SOM.makeSGM lrf (fromIntegral maxSz) dt False df af
        lrf = toLearningFunction e
        df = diff t
        af = adjust t

instance (Eq p, Eq t) => Eq (GeneticSOM p t) where
  (==) x y = (SOM.toMap . _patternMap $ x) == (SOM.toMap . _patternMap $ y)
             && _learningParams x == _learningParams y
             && _tweaker x == _tweaker y

instance (Show p, Show t) => Show (GeneticSOM p t) where
  show x = "GeneticSOM (" ++ showSGM s e t
             ++ ") (" ++ show e
             ++ ") (" ++ show t ++ ")"
    where s = _patternMap x
          e = _learningParams x
          t = _tweaker x

-- | Formats a genetic SOM for display.
showSGM
  :: (Show p, Show t)
    => SOM.SGM Word64 UIDouble Label p -> LearningParams -> t -> String
showSGM s e t = "SGM (" ++ show (SOM.toMap s)
                   ++ ") (toLearningFunction (" ++ show e
                   ++ ")) " ++ show (SOM.maxSize s)
                   ++ " " ++ show (SOM.diffThreshold s)
                   ++ " " ++ show (SOM.allowDeletion s)
                   ++ " (diff (" ++ show t
                   ++ ")) (adjust (" ++ show t
                   ++ ")) " ++ show (SOM.nextIndex s)

instance (S.Serialize p, S.Serialize t, Tweaker t, p ~ Pattern t)
    => S.Serialize (GeneticSOM p t) where
  put s = S.put (SOM.toMap . _patternMap $ s)
            >> S.put (_learningParams s)
            >> S.put (SOM.maxSize . _patternMap $ s)
            >> S.put (SOM.diffThreshold . _patternMap $ s)
            >> S.put (_tweaker s)
            >> S.put (SOM.nextIndex . _patternMap $ s)
  get = do
    gm <- S.get
    lps <- S.get
    maxSz <- S.get
    dt <- S.get
    tr <- S.get
    kNext <- S.get
    let lrf = toLearningFunction lps
    let df = diff tr
    let af = adjust tr
    let s = SOM.SGM gm lrf maxSz dt False df af kNext
    return $ GeneticSOM s lps tr

instance (G.Genetic p, G.Genetic t, Tweaker t, p ~ Pattern t)
    => G.Genetic (GeneticSOM p t) where
  put s = G.put (M.elems . SOM.toMap . _patternMap $ s)
            >> G.put (_learningParams s)
            >> G.put (fromIntegral . SOM.maxSize . _patternMap $ s :: Word64)
            >> G.put (SOM.diffThreshold . _patternMap $ s)
            >> G.put (_tweaker s)
  get = do
    nodes <- G.get
    let newLabels = [minBound..] :: [Word64]
    let gm = M.fromList . zip newLabels <$> nodes
    lps <- G.get
    maxSz <- fmap fromIntegral <$> (G.get :: G.Reader (Either [String] Word64))
    dt <- G.get
    tr <- G.get
    let lrf = toLearningFunction <$> lps
    let df = diff <$> tr
    let af = adjust <$> tr
    let kNext = toEnum . length <$> nodes
    let s = SOM.SGM <$> gm <*> lrf <*> maxSz <*> dt <*> pure False <*> df <*> af <*> kNext
    return $ GeneticSOM <$> s <*> lps <*> tr

instance (Diploid p, Diploid t, Tweaker t, p ~ Pattern t)
    => Diploid (GeneticSOM p t) where
  express x y = GeneticSOM s lps tr
    where s = SOM.SGM gm lrf maxSz dt False df af kNext
          gm = M.fromList $
                 express (M.toList . SOM.toMap . _patternMap $ x)
                         (M.toList . SOM.toMap . _patternMap $ y)
          lrf = toLearningFunction lps
          maxSz = express (SOM.maxSize . _patternMap $ x)
                          (SOM.maxSize . _patternMap $ y)
          dt = express (SOM.diffThreshold . _patternMap $ x)
                       (SOM.diffThreshold . _patternMap $ y)
          df = diff tr
          af = adjust tr
          kNext = express (SOM.nextIndex . _patternMap $ x)
                          (SOM.nextIndex . _patternMap $ y)
          lps = express (_learningParams x) (_learningParams y)
          tr = express (_tweaker x) (_tweaker y)

instance Statistical (GeneticSOM p t) where
  stats s =
    (iStat "num models" . numModels $ s)
      :(iStat "max. size" . SOM.maxSize . _patternMap $ s)
      :(dStat "threshold" . SOM.diffThreshold . _patternMap $ s)
      :(iStat "SQ" . schemaQuality $ s)
      :(stats . _learningParams $ s)

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

-- | Internal method.
discrimination :: Integral a => [a] -> Int
discrimination xs = length $ filter (>k) xs
  where k = sum xs `div` fromIntegral (2 * length xs)

-- | Returns @True@ if the SOM has no models, @False@ otherwise.
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
counterMap :: GeneticSOM p t -> M.Map Label Word64
counterMap = SOM.counterMap . _patternMap

-- | @'classify' s p@ identifies the model @s@ that most closely
--   matches the pattern @p@.
--   Returns the ID of the node with the best matching model,
--   the "novelty" of the the pattern @p@ (difference between it and
--   the best matching model),
--   the differences between the input and each model in the SOM,
--   and the (possibly updated) SOM.
classify
  :: GeneticSOM p t -> p
    -> (Label, Difference, [(Label, Difference)])
classify gs p = (bmu, bmuDiff, diffs)
  where (bmu, bmuDiff, diffs) = SOM.classify s p
        s = view patternMap gs

trainAndClassify
  :: GeneticSOM p t -> p
    -> (Label, Difference, [(Label, Difference)], GeneticSOM p t)
trainAndClassify gs p = (bmu, bmuDiff, diffs, gs')
  where (bmu, bmuDiff, diffs, s') = SOM.trainAndClassify s p
        s = view patternMap gs
        gs' = set patternMap s' gs

-- | @'train' s p@ identifies the model in @s@ that most closely
--   matches @p@, and updates it to be a somewhat better match.
train :: GeneticSOM p t -> p -> GeneticSOM p t
train gs p = set patternMap s' gs
  where s' = SOM.train s p
        s = view patternMap gs


-- | Returns @True@ if the SOM has a model with the specified label;
--   returns @False@ otherwise.
hasLabel :: GeneticSOM p t -> Label -> Bool
hasLabel gs l = M.member l (modelMap gs)
