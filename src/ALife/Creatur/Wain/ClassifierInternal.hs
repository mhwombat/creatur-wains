------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ClassifierInternal
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
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

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import qualified ALife.Creatur.Wain.GeneticSOM           as S
import           ALife.Creatur.Wain.Pattern              (Pattern)
import           ALife.Creatur.Wain.Pretty               (Pretty, pretty)
import           Control.DeepSeq                         (NFData)
import           Data.List                               (foldl')
import qualified Data.Map.Strict                         as M
import           GHC.Generics                            (Generic)

-- | Maintains a set of models to represent the input patterns
--   encountered, and classifies the input patterns according to the
--   models.
type Classifier p = S.GeneticSOM p

-- | Detailed information about how a classification was made.
data ClassifierReport p =
  ClassifierReport
    {
      cLearningRate :: UI.UIDouble,
      cDetails      :: [S.ClassificationDetail p]
    } deriving (Generic, Show, NFData)

-- | Generates a human readable summary of a classification.
prettyClassifierReport :: Pretty p => ClassifierReport p -> [String]
prettyClassifierReport r =
    ("classifier learning rate: " ++ pretty (cLearningRate r))
    : concatMap S.prettyClassificationDetail (cDetails r)

-- | Updates the classifier models based on the stimulus (set of
--   input patterns).
--   Returns the classification report and the updated classifier.
classifySetAndTrain
  :: Pattern p
  => Classifier p -> [p] -> (ClassifierReport p, Classifier p)
classifySetAndTrain c ps = (report, c')
  where (details, c') = foldl' classifyNextAndTrain ([], c) ps
        report = ClassifierReport
                   {
                     cLearningRate = S.currentLearningRate c,
                     cDetails = reverse details
                   }

-- | Internal method
classifyNextAndTrain
  :: Pattern p
  => ([S.ClassificationDetail p], Classifier p) -> p
    -> ([S.ClassificationDetail p], Classifier p)
classifyNextAndTrain (details, c) p = (detail:details, c')
  where (detail, c') = S.trainAndClassify c p

-- | Returns the label of the node whose model best matched each input
--   pattern.
bmus :: ClassifierReport p -> [S.Label]
bmus = map S.cBmu . cDetails

-- | For each input pattern, returns the labels for all nodes in
--   the classifier, paired with the difference between the
--   input pattern and the corresponding model.
diffs :: ClassifierReport p -> [[(S.Label, UI.UIDouble)]]
diffs = map diffs' . cDetails

diffs' :: S.ClassificationDetail p -> [(S.Label, UI.UIDouble)]
diffs' = M.toList . M.map snd . S.cDetails

-- | Returns the current node labels
labels :: Classifier p -> [S.Label]
labels = M.keys . S.modelMap

-- | Teaches the classifier a set of input patterns, along with the
--   labels for each pattern.
imprintSet
  :: Pattern p
  => Classifier p -> [(S.Label, p)] -> ([S.ImprintDetail p], Classifier p)
imprintSet c lps = (reverse details, c')
  where (details, c') = foldl' imprintNext ([], c) lps

-- | Internal method
imprintNext
  :: Pattern p
  => ([S.ImprintDetail p], Classifier p)
    -> (S.Label, p)
    -> ([S.ImprintDetail p], Classifier p)
imprintNext (details, c) (l, p) = (detail:details, c')
  where (detail, c') = S.imprint c l p
