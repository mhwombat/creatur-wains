------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.GeneticSOM
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides a self-organising map that can be genetically configured.
--
------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ALife.Creatur.Wain.GeneticSOM
  (
    -- * Construction
    GeneticSOM,
    -- * Deconstruction
    filter,
    hasLabel,
    currentLearningRate,
    schemaQuality,
    discrimination,
    -- * Learning and classification
    -- classify,
    trainAndClassify,
    imprint,
    -- * Other
    Label,
    ClassificationDetail(..),
    prettyClassificationDetail,
    prettyClassificationMoreDetail,
    ImprintDetail(..),
    prettyImprintDetail,
    adjNovelty -- TODO: move to GeneticSOMInternal, should only be used for testing
  ) where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import qualified ALife.Creatur.Genetics.BRGCWord8        as G
import           ALife.Creatur.Genetics.Diploid          (Diploid, express)
import           ALife.Creatur.Wain.Pretty               (Pretty, pretty)
import           ALife.Creatur.Wain.Report               (Report, report)
import           ALife.Creatur.Wain.Statistics           (Statistical, iStat,
                                                          kvToIStats, prefix,
                                                          stats)
import           Control.DeepSeq                         (NFData)
import qualified Data.Datamining.Clustering.SGM4         as SOM
import qualified Data.Map.Strict                         as M
import qualified Data.Serialize                          as S
import           Data.Word                               (Word32)
import           GHC.Generics                            (Generic)

-- | A unique identifier for a model in a SOM.
type Label = Word32

instance (G.Genetic k, Ord k, G.Genetic p)
    => G.Genetic (M.Map k p) where
  put gm = G.put (M.toList gm)
  get = do
    xs <- G.get
    return $ M.fromList <$> xs

instance (Ord k, Diploid p) => Diploid (M.Map k p) where
  express gm1 gm2 = M.fromList . zip ks $ vs
    where ks = M.keys gm1
          vs = express (M.elems gm1) (M.elems gm2)

-- | A Simplified Self-Organising Map (SOM).
--   @p@ is the type of the input patterns and models.
--   @t@ is the type of the tweaker.
type GeneticSOM t p = SOM.SGM t Word32 Label p

instance (S.Serialize p, S.Serialize t) => S.Serialize (GeneticSOM t p)
instance (G.Genetic p, G.Genetic t) => G.Genetic (GeneticSOM t p)
instance (Diploid p, Diploid t) => Diploid (GeneticSOM t p)

instance (SOM.Adjuster t, Statistical t) => Statistical (GeneticSOM t p) where
  stats s =
    (iStat "num models" . SOM.size $ s)
      : (iStat "max. size" . SOM.capacity $ s)
      : (iStat "SQ" . schemaQuality $ s)
      : (map (prefix "counter") . kvToIStats . M.toAscList . SOM.counterMap $ s)
      ++ (stats . SOM.adjuster $ s)


-- | Returns @True@ if the SOM has a model with the specified label;
--   returns @False@ otherwise.
hasLabel :: GeneticSOM t p -> Label -> Bool
hasLabel s l = M.member l (SOM.modelMap s)


-- | Returns the SOM's current learning rate.
currentLearningRate
  :: (SOM.Adjuster t, SOM.MetricType t ~ UI.UIDouble, SOM.TimeType t ~ Word32)
  => GeneticSOM t p -> UI.UIDouble
currentLearningRate s = (SOM.learningRate $ SOM.adjuster s) (SOM.time s)

-- | Measures the quality of the classification system represented by
--   the SOM, and returns the result.
schemaQuality :: SOM.Adjuster t => GeneticSOM t p -> Int
schemaQuality = discrimination . M.elems . SOM.counterMap

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
        cNovelty    :: UI.UIDouble,
        -- | A measure of how novel the input pattern was to the wain,
        --   adjusted based on the age of the wain.
        cAdjNovelty :: Int,
        -- | Even more details about the classification
        cDetails    :: M.Map Label (p, UI.UIDouble)
      } deriving (Read, Show, Generic, NFData)

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
  => M.Map Label (p, UI.UIDouble) -> [String]
prettyClassificationMoreDetail = map f . M.toList
  where f (l, (p, d)) = show l ++ " " ++ pretty p ++ " " ++ pretty d

trainAndClassify
  :: (SOM.Adjuster t, SOM.PatternType t ~ p,
     SOM.MetricType t ~ UI.UIDouble, SOM.TimeType t ~ Word32)
  => GeneticSOM t p -> p -> (ClassificationDetail p, GeneticSOM t p)
trainAndClassify gs p = (detail, gs')
  where (bmu, novelty, rs, gs') = SOM.trainAndClassify gs p
        age = SOM.time gs
        detail = ClassificationDetail
                   {
                     cPattern = p,
                     cBmu = bmu,
                     cBmuModel = gs' `SOM.modelAt` bmu,
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
      } deriving (Read, Show, Generic, NFData)

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
imprint
  :: (SOM.Adjuster t, SOM.PatternType t ~ p, SOM.TimeType t ~ Word32,
    SOM.MetricType t ~ UI.UIDouble)
  => GeneticSOM t p -> Label -> p -> (ImprintDetail p, GeneticSOM t p)
imprint gs l p
  | existing          = (detail, gs')
  | SOM.atCapacity gs = (detail0, gs)
  | otherwise         = (detail, gs')
  where gs' = SOM.imprint gs l p
        existing = gs `hasLabel` l
        detail0 = ImprintDetail
                   {
                     iPattern = p,
                     iLabel = l,
                     iNew = not existing,
                     iModel = Nothing
                   }
        detail = detail0 { iModel = Just (gs' `SOM.modelAt` l) }

-- | Calculates the novelty of the input by scaling the BMU difference
--   according to the age of the classifier (wain).
adjNovelty :: UI.UIDouble -> Word32 -> Int
adjNovelty d a = round $ UI.wide d * fromIntegral a

instance (SOM.Adjuster t, Report t, Pretty p) => Report (GeneticSOM t p) where
  report s = [ "max models: " ++ pretty (SOM.capacity s),
               "num models: " ++ pretty (SOM.size s),
               "SQ: " ++ pretty (schemaQuality s),
               "counts: " ++ pretty (SOM.counterMap s) ]
             ++ report (SOM.adjuster s)
             ++ map f ms
    where ms = M.toList . SOM.modelMap $ s
          f (l, r) = pretty l ++ ": " ++ pretty r
