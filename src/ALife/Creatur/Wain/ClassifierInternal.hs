------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ClassifierInternal
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private Classifier internals.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ALife.Creatur.Wain.ClassifierInternal where

import ALife.Creatur.Gene.Numeric.UnitInterval qualified as UI
import ALife.Creatur.Wain.GeneticSOM           qualified as GSOM
import ALife.Creatur.Wain.Pretty               (Pretty, pretty)
import Control.DeepSeq                         (NFData)
import Data.Datamining.Clustering.SGM4         qualified as SOM
import Data.List                               (foldl')
import Data.Map.Strict                         qualified as M
import Data.Word                               (Word32)
import GHC.Generics                            (Generic)

-- | Maintains a set of models to represent the input patterns
--   encountered, and classifies the input patterns according to the
--   models.
type Classifier t p = GSOM.GeneticSOM t p

-- | Detailed information about how a classification was made.
data ClassifierReport p =
  ClassifierReport
    {
      cLearningRate :: UI.Double,
      cDetails      :: [GSOM.ClassificationDetail p]
    } deriving (Generic, Show, NFData)

-- | Generates a human readable summary of a classification.
prettyClassifierReport :: Pretty p => ClassifierReport p -> [String]
prettyClassifierReport r =
    ("classifier learning rate: " ++ pretty (cLearningRate r))
    : concatMap GSOM.prettyClassificationDetail (cDetails r)

-- | Updates the classifier models based on the stimulus (set of
--   input patterns).
--   Returns the classification report and the updated classifier.
classifySetAndTrain
  :: (SOM.Adjuster t, SOM.PatternType t ~ p,
     SOM.MetricType t ~ UI.Double, SOM.TimeType t ~ Word32)
  => Classifier t p -> [p] -> (ClassifierReport p, Classifier t p)
classifySetAndTrain c ps = (report, c')
  where (details, c') = foldl' classifyNextAndTrain ([], c) ps
        report = ClassifierReport
                   {
                     cLearningRate = GSOM.currentLearningRate c,
                     cDetails = reverse details
                   }

-- | Internal method
classifyNextAndTrain
  :: (SOM.Adjuster t, SOM.PatternType t ~ p,
     SOM.MetricType t ~ UI.Double, SOM.TimeType t ~ Word32)
  => ([GSOM.ClassificationDetail p], Classifier t p) -> p
    -> ([GSOM.ClassificationDetail p], Classifier t p)
classifyNextAndTrain (details, c) p = (detail:details, c')
  where (detail, c') = GSOM.trainAndClassify c p

-- | Returns the label of the node whose model best matched each input
--   pattern.
bmus :: ClassifierReport p -> [GSOM.Label]
bmus = map GSOM.cBmu . cDetails

-- | For each input pattern, returns the labels for all nodes in
--   the classifier, paired with the difference between the
--   input pattern and the corresponding model.
diffs :: ClassifierReport p -> [[(GSOM.Label, UI.Double)]]
diffs = map diffs' . cDetails

diffs' :: GSOM.ClassificationDetail p -> [(GSOM.Label, UI.Double)]
diffs' = M.toList . M.map snd . GSOM.cDetails

-- | Returns the current node labels
labels :: Classifier t p -> [GSOM.Label]
labels = M.keys . SOM.modelMap

-- | Teaches the classifier a set of input patterns, along with the
--   labels for each pattern.
imprintSet
  :: (SOM.Adjuster t, SOM.PatternType t ~ p,
     SOM.MetricType t ~ UI.Double, SOM.TimeType t ~ Word32)
  => Classifier t p -> [(GSOM.Label, p)] -> ([GSOM.ImprintDetail p], Classifier t p)
imprintSet c lps = (reverse details, c')
  where (details, c') = foldl' imprintNext ([], c) lps

-- | Internal method
imprintNext
  :: (SOM.Adjuster t, SOM.PatternType t ~ p,
     SOM.MetricType t ~ UI.Double, SOM.TimeType t ~ Word32)
  => ([GSOM.ImprintDetail p], Classifier t p)
    -> (GSOM.Label, p)
    -> ([GSOM.ImprintDetail p], Classifier t p)
imprintNext (details, c) (l, p) = (detail:details, c')
  where (detail, c') = GSOM.imprint c l p
