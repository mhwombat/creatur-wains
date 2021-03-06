------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.GeneticSOMInternal
-- Copyright   :  (c) Amy de Buitléir 2013-2019
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
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.GeneticSOMInternal where

import           ALife.Creatur.Genetics.BRGCWord8        (Genetic)
import qualified ALife.Creatur.Genetics.BRGCWord8        as G
import           ALife.Creatur.Genetics.Diploid          (Diploid, express)
import           ALife.Creatur.Wain.Pretty               (Pretty, pretty)
import           ALife.Creatur.Wain.Statistics
    (Statistical, dStat, iStat, kvToIStats, prefix, stats)
import           ALife.Creatur.Wain.UnitInterval
    (UIDouble, doubleToUI, uiToDouble)
import           ALife.Creatur.Wain.Util                 (inRange, intersection)
import           Control.DeepSeq                         (NFData)
import           Control.Lens
import           Control.Monad.Random
    (Rand, RandomGen, getRandomR)
import qualified Data.Datamining.Clustering.SGM4Internal as SOM
import qualified Data.Map.Strict                         as M
import qualified Data.Serialize                          as S
import           Data.Word                               (Word64)
import           GHC.Generics                            (Generic)

-- | A unique identifier for a model in a SOM.
type Label = Word64

-- | A measure of the difference between an input pattern and a model.
type Difference = UIDouble

-- | Private constructor.
--   @'LearningParams' r0 rf tf@ defines the shape of the learning
--   function.
--   When t = 0, the learning rate is r0.
--   Over time the learning rate decays,
--   so that when t = tf, the learning rate is rf.
--   Normally the parameters are chosen such that:
--     0 < r0 <= 1
--     0 < rf <= r0
data LearningParams = LearningParams UIDouble UIDouble Word64
  deriving (Eq, Show, Read, Pretty, Generic, NFData)

-- | @'mkLearningParams' r0 rf tf@ defines the shape of the learning
--   function.
--   When t = 0, the learning rate is r0.
--   Over time the learning rate decays exponentially,
--   so that when t = tf, the learning rate is rf.
--   Normally the parameters are chosen such that:
--     0 < r0 <= 1
--     0 < rf <= r0
mkLearningParams
  :: UIDouble -> UIDouble -> Word64 -> Either [String] LearningParams
mkLearningParams r0 rf tf
  | r0 == 0   = Left ["r0 must be > 0"]
  | rf > r0   = Left ["rf must be <= r0"]
  | otherwise = Right $ LearningParams r0 rf tf

instance G.Genetic LearningParams where
  put (LearningParams r0 rf tf)
    = G.put r0 >> G.put rf >> G.put tf
  get = do
    r0 <- G.get
    rf <- G.get
    tf <- G.get
    -- Use the safe constructor!
    case (mkLearningParams <$> r0 <*> rf <*> tf) of
      Left msgs -> return $ Left msgs
      Right p   -> return p

instance S.Serialize LearningParams
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
  where r = r0' * exp (-d*t')
        r0' = uiToDouble r0
        rf' = uiToDouble rf
        t' = fromIntegral t
        tf' = fromIntegral tf
        d = log (r0'/rf') / tf'

-- | A set of parameters to constrain the result when generating
--   random learning functions.
data LearningParamRanges = LearningParamRanges
  {
    -- | The range from which the initial learning rate (at t=0)
    --   should be chosen.
    _r0Range :: (UIDouble, UIDouble),
    -- | The range from which the final learning rate (at t=@tf@)
    --   should be chosen.
    _rfRange :: (UIDouble, UIDouble),
    -- | The range from which the final time should be chosen.
    _tfRange :: (Word64, Word64)
  } deriving (Show, Read)
makeLenses ''LearningParamRanges

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
widestLearningParamRanges :: LearningParamRanges
widestLearningParamRanges =
  LearningParamRanges r0RangeLimits rfRangeLimits tfRangeLimits

-- | @'randomLearningParams'
--   ('LearningParamRanges' r0Range rfRange tRange)@
--   returns a random decaying function that can be used as the
--   learning function for an SGM.
--   The parameters of the gaussian will be chosen such that:
--
--   * r0 is in r0Range, but also 0 < r0 <= 1
--   * rf is in rfRange, but also 0 < rf <= 1
--   * tf is in tfRange, but also 0 < tf
randomLearningParams
  :: RandomGen g
    => LearningParamRanges
      -> Rand g LearningParams
randomLearningParams p = do
  r0 <- getRandomR . intersection r0RangeLimits . _r0Range $ p
  rf <- getRandomR . intersection (0, r0) . intersection rfRangeLimits . _rfRange $ p
  tf <- getRandomR . intersection tfRangeLimits . _tfRange $ p
  let Right x = mkLearningParams r0 rf tf
  return x

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
      _patternMap     :: SOM.SGM Word64 UIDouble Label p,
      -- | The parameters that define the learning function
      _learningParams :: LearningParams,
      -- | The object responsible for comparing and adjusting models.
      _tweaker        :: t
    } deriving (Generic, NFData)
makeLenses ''GeneticSOM

-- | @'buildGeneticSOM' e n t@ returns a genetic SOM, using an
--   exponential function defined by the parameters @e@ as a learning
--   function, maximum number of models @n@, and "tweaker" @t@.
buildGeneticSOM
  :: (Tweaker t, p ~ Pattern t)
    => LearningParams -> Word64 -> t -> GeneticSOM p t
buildGeneticSOM e maxSz t
  = GeneticSOM som e t
  where som = SOM.makeSGM lrf (fromIntegral maxSz) df af
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
                   ++ " (diff (" ++ show t
                   ++ ")) (adjust (" ++ show t
                   ++ ")) " ++ show (SOM.nextIndex s)

instance (S.Serialize p, S.Serialize t, Tweaker t, p ~ Pattern t)
    => S.Serialize (GeneticSOM p t) where
  put s = S.put (SOM.toMap . _patternMap $ s)
            >> S.put (_learningParams s)
            >> S.put (SOM.maxSize . _patternMap $ s)
            >> S.put (_tweaker s)
            >> S.put (SOM.nextIndex . _patternMap $ s)
  get = do
    gm <- S.get
    lps <- S.get
    maxSz <- S.get
    tr <- S.get
    kNext <- S.get
    let lrf = toLearningFunction lps
    let df = diff tr
    let af = adjust tr
    let s = SOM.SGM gm lrf maxSz df af kNext
    return $ GeneticSOM s lps tr

instance (G.Genetic p, G.Genetic t, Tweaker t, p ~ Pattern t)
    => G.Genetic (GeneticSOM p t) where
  put s = G.put (M.elems . SOM.toMap . _patternMap $ s)
            >> G.put (_learningParams s)
            >> G.put (fromIntegral . SOM.maxSize . _patternMap $ s :: Word64)
            >> G.put (_tweaker s)
  get = do
    nodes <- G.get
    let newLabels = [minBound..] :: [Word64]
    let gm = M.fromList . zip newLabels <$> nodes
    lps <- G.get
    maxSz <- fmap fromIntegral <$> (G.get :: G.Reader (Either [String] Word64))
    tr <- G.get
    let lrf = toLearningFunction <$> lps
    let df = diff <$> tr
    let af = adjust <$> tr
    let kNext = toEnum . length <$> nodes
    let s = SOM.SGM <$> gm <*> lrf <*> maxSz <*> df <*> af <*> kNext
    return $ GeneticSOM <$> s <*> lps <*> tr

instance (Diploid p, Diploid t, Tweaker t, p ~ Pattern t)
    => Diploid (GeneticSOM p t) where
  express x y = GeneticSOM s lps tr
    where s = SOM.SGM gm lrf maxSz df af kNext
          gm = M.fromList $
                 express (M.toList . SOM.toMap . _patternMap $ x)
                         (M.toList . SOM.toMap . _patternMap $ y)
          lrf = toLearningFunction lps
          maxSz = express (SOM.maxSize . _patternMap $ x)
                          (SOM.maxSize . _patternMap $ y)
          df = diff tr
          af = adjust tr
          kNext = express (SOM.nextIndex . _patternMap $ x)
                          (SOM.nextIndex . _patternMap $ y)
          lps = express (_learningParams x) (_learningParams y)
          tr = express (_tweaker x) (_tweaker y)

instance (Statistical t, Statistical [(Label, p)])
  => Statistical (GeneticSOM p t) where
  stats s =
    (iStat "num models" . numModels $ s)
      :(iStat "max. size" . SOM.maxSize . _patternMap $ s)
      :(iStat "SQ" . schemaQuality $ s)
      :(stats . _learningParams $ s)
      -- ++ (map (prefix "model") . stats . M.toAscList . modelMap $ s)
      ++ (map (prefix "counter") . kvToIStats . M.toAscList . counterMap $ s)
      ++ (stats . _tweaker $ s)

-- | Returns @True@ if the SOM has no models, @False@ otherwise.
isEmpty :: GeneticSOM p t -> Bool
isEmpty = SOM.isEmpty . _patternMap

-- | Returns True if the SOM is full, False otherwise.
atCapacity
  :: GeneticSOM p t -> Bool
atCapacity (GeneticSOM s _ _) = SOM.atCapacity s


-- | The maximum number of models this SOM can hold.
maxSize :: GeneticSOM p t -> Int
maxSize = SOM.maxSize . _patternMap

-- | Returns the number of models in the SOM.
numModels
  :: GeneticSOM p t -> Int
numModels (GeneticSOM s _ _) = SOM.numModels s

-- -- | Returns the models in the SOM.
-- models
--   :: GeneticSOM p t -> [p]
-- models (GeneticSOM s _ _) = C.models s

-- | Returns a map from node ID to model.
modelMap :: GeneticSOM p t -> M.Map Label p
modelMap gs = SOM.modelMap s
  where s = _patternMap gs

-- | Returns a map from node label to counter (number of times the
--   node's model has been the closest match to an input pattern).
counterMap :: GeneticSOM p t -> M.Map Label Word64
counterMap = SOM.counterMap . _patternMap

-- | Returns @True@ if the SOM has a model with the specified label;
--   returns @False@ otherwise.
hasLabel :: GeneticSOM p t -> Label -> Bool
hasLabel gs l = M.member l (modelMap gs)

-- | Returns the model at the specified location in the SOM.
modelAt :: GeneticSOM p t -> Label -> p
modelAt gs k = (_patternMap gs) `SOM.modelAt` k

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

data ClassificationDetail p
  = ClassificationDetail
      {
        -- | The input pattern
        cPattern    :: p,
        -- | The label of the node that best matches the input
        cBmu        :: Label,
        -- | The BMU's model
        cBmuModel   :: p,
        -- | A measure of how novel the input pattern was to the wain.
        --   It is the difference between the input pattern and the
        --   closest model prior to any training or addition of models.
        cNovelty    :: Difference,
        -- | A measure of how novel the input pattern was to the wain,
        --   adjusted based on the age of the wain.
        cAdjNovelty :: Int,
        -- | Even more details about the classification
        cDetails    :: M.Map Label (p, Difference)
      } deriving (Show, Generic, NFData)

prettyClassificationDetail
  :: Pretty p
  => ClassificationDetail p -> [String]
prettyClassificationDetail r =
  [
    "input pattern " ++ pretty (cPattern r),
    "classifier BMU: " ++ show (cBmu r)
      ++ " novelty: " ++ show (cNovelty r)
      ++ " adjusted novelty: " ++ show (cAdjNovelty r),
    "classification details (label, model, diff):"
  ] ++ prettyClassificationMoreDetail (cDetails r)

prettyClassificationMoreDetail
  :: Pretty p
  => M.Map Label (p, Difference) -> [String]
prettyClassificationMoreDetail = map f . M.toList
  where f (l, (p, d)) = show l ++ " " ++ pretty p ++ " " ++ pretty d

-- | @'classify' s p@ identifies the model @s@ that most closely
--   matches the pattern @p@.
--   It makes no changes to the GSOM.
--   (I.e., no training occurs, no new models are added,
--   the counts are not updated.)
--   For this reason, it should only be used for testing.
classify :: GeneticSOM p t -> p -> ClassificationDetail p
classify gs p = detail
  where (bmu, novelty, rs) = SOM.classify s p
        s = view patternMap gs
        a = SOM.time $ _patternMap gs
        detail = ClassificationDetail
                   {
                     cPattern = p,
                     cBmu = bmu,
                     cBmuModel = gs `modelAt` bmu,
                     cNovelty = novelty,
                     cAdjNovelty = adjNovelty novelty a,
                     cDetails = rs
                   }

trainAndClassify
  :: GeneticSOM p t -> p -> (ClassificationDetail p, GeneticSOM p t)
trainAndClassify gs p = (detail, gs')
  where (bmu, novelty, rs, s') = SOM.trainAndClassify s p
        s = view patternMap gs
        gs' = set patternMap s' gs
        age = SOM.time $ _patternMap gs
        detail = ClassificationDetail
                   {
                     cPattern = p,
                     cBmu = bmu,
                     cBmuModel = gs' `modelAt` bmu,
                     cNovelty = novelty,
                     cAdjNovelty = adjNovelty novelty age,
                     cDetails = rs
                   }

data ImprintDetail p
  = ImprintDetail
      {
        -- | The input pattern
        iPattern :: p,
        -- | The label of the node that learned the pattern
        iLabel   :: Label,
        -- | Is the pattern new (imprinted) or old (reinforced)
        iNew     :: Bool,
        -- | The resulting model
        --   May not be identical to the input pattern if a node with
        --   that label already existed.
        iModel   :: Maybe p
      } deriving (Show, Generic, NFData)

prettyImprintDetail :: Pretty p => ImprintDetail p -> [String]
prettyImprintDetail r =
  [
    "Input pattern " ++ pretty (iPattern r),
    "  classifier label: " ++ show (iLabel r),
    msg ++ pretty (iModel r)
  ]
  where msg = if iNew r
                then "  imprinted new model: "
                else "  reinforced existing model: "

-- | Teaches the classifier a pattern and a label for that pattern.
imprint :: GeneticSOM p t -> Label -> p -> (ImprintDetail p, GeneticSOM p t)
imprint gs l p
  | existing      = (detail, gs')
  | atCapacity gs = (detail0, gs)
  | otherwise     = (detail, gs')
  where gs' = over patternMap (\s -> SOM.imprint s l p) gs
        existing = gs `hasLabel` l
        detail0 = ImprintDetail
                   {
                     iPattern = p,
                     iLabel = l,
                     iNew = not existing,
                     iModel = Nothing
                   }
        detail = detail0 { iModel = Just (gs' `modelAt` l) }

-- | Calculates the novelty of the input by scaling the BMU difference
--   according to the age of the classifier (wain).
adjNovelty :: Difference -> Word64 -> Int
adjNovelty d a = round $ uiToDouble d * fromIntegral a

-- -- | @'train' s p@ identifies the model in @s@ that most closely
-- --   matches @p@, and updates it to be a somewhat better match.
-- train :: GeneticSOM p t -> p -> GeneticSOM p t
-- train gs p = set patternMap s' gs
--   where s' = SOM.train s p
--         s = view patternMap gs

-- | @'filterByPattern' s f@ applies the function @f@ to all patterns
--   in @s@ where all of the
--   patterns for which, and removes those for which the function @f@
--   returns False.
filterByPattern :: (p -> Bool) -> GeneticSOM p t -> GeneticSOM p t
filterByPattern f s = over patternMap (SOM.filter f) s
