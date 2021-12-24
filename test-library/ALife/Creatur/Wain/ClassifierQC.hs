------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ClassifierQC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.ClassifierQC
  (
    test,
    TestClassifierTweaker(..),
    TestClassifier
  ) where

import qualified ALife.Creatur.Gene.Test                 as GT
import qualified ALife.Creatur.Genetics.BRGCWord8        as W8
import           ALife.Creatur.Genetics.Diploid          (Diploid)
import           ALife.Creatur.Wain.ClassifierInternal
import qualified ALife.Creatur.Wain.GeneticSOMInternal   as GSOM
import           ALife.Creatur.Wain.GeneticSOMQC         (equivGSOM,
                                                          sizedArbGeneticSOM)
import           ALife.Creatur.Wain.Statistics           (Statistical (..))
import           Control.DeepSeq                         (NFData, deepseq)
import           Control.Lens
import qualified Data.Datamining.Clustering.SGM4Internal as SOM
import qualified Data.Map.Lazy                           as M
import           Data.Serialize                          (Serialize)
import           GHC.Generics                            (Generic)
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2    (testProperty)
import           Test.QuickCheck                         hiding (classify,
                                                          maxSize)

data TestClassifierTweaker = TestClassifierTweaker
  deriving (Eq, Show, Generic, Serialize, W8.Genetic, Diploid, NFData)

instance GSOM.Tweaker TestClassifierTweaker where
  type Pattern TestClassifierTweaker = GT.TestPattern
  diff _ = GT.testPatternDiff
  adjust _ = GT.makeTestPatternSimilar

instance Arbitrary TestClassifierTweaker where
  arbitrary = return TestClassifierTweaker

type TestClassifier = Classifier GT.TestPattern TestClassifierTweaker

instance Arbitrary TestClassifier where
  arbitrary = sized (sizedArbGeneticSOM arbitrary)

instance Statistical TestClassifierTweaker where
  stats _ = []

classifySetAndTrain_label_count
  :: TestClassifier -> [GT.TestPattern] -> Bool
classifySetAndTrain_label_count c ps = length xs == length ps
  where (r, _) = classifySetAndTrain c ps
        xs = bmus r

prop_classifier_behaves_like_sgm
  :: TestClassifier -> GT.TestPattern -> Bool
prop_classifier_behaves_like_sgm c p = cModels == sModels
  where s = view GSOM.patternMap c
        (_, c') = classifySetAndTrain c [p]
        (_, _, _, s') = SOM.trainAndClassify s p
        cModels = M.elems . GSOM.modelMap $ c'
        sModels = M.elems . SOM.modelMap $ s'

prop_classifier_behaves_like_sgm2
  :: TestClassifier -> GT.TestPattern -> Bool
prop_classifier_behaves_like_sgm2 c p = cLds == [sLds]
  where s = view GSOM.patternMap c
        (report, _) = classifySetAndTrain c [p]
        cLds = bmus report
        (sLds, _, _, _) = SOM.trainAndClassify s p

prop_classifier_behaves_like_sgm3
  :: TestClassifier -> [GT.TestPattern] -> Bool
prop_classifier_behaves_like_sgm3 c ps = cModels == sModels
  where s = view GSOM.patternMap c
        (_, c') = classifySetAndTrain c ps
        s' = SOM.trainBatch s ps
        cModels = M.elems . GSOM.modelMap $ c'
        sModels = M.elems . SOM.modelMap $ s'

prop_classifySetAndTrain_never_causes_error
  :: TestClassifier -> [GT.TestPattern] -> Bool
prop_classifySetAndTrain_never_causes_error c ps
  = deepseq (classifySetAndTrain c ps) True

-- prop_prettyClassifierReport_never_causes_error
--   :: TestClassifier -> [TestPattern] -> Bool
-- prop_prettyClassifierReport_never_causes_error c ps
--   = deepseq x True
--   where (r, _) = classifySetAndTrain c ps
--         x = prettyClassifierReport r

prop_imprintSet_never_causes_error
  :: TestClassifier -> [(GSOM.Label, GT.TestPattern)] -> Bool
prop_imprintSet_never_causes_error c lps
  = deepseq (imprintSet c lps) True

test :: Test
test = testGroup "ALife.Creatur.Wain.ClassifierQC"
  [
    testProperty "prop_serialize_round_trippable - Classifier"
      (GT.prop_serialize_round_trippable :: TestClassifier -> Bool),
    testProperty "prop_genetic_round_trippable - Classifier"
      (GT.prop_genetic_round_trippable equivGSOM :: TestClassifier -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - Classifier"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestClassifier -> Bool),
    testProperty "prop_diploid_identity - Classifier"
      (GT.prop_diploid_identity (==) :: TestClassifier -> Bool),
    -- testProperty "prop_show_read_round_trippable - Classifier"
    --   (GT.prop_show_read_round_trippable (==) :: TestClassifier -> Bool),
    testProperty "prop_diploid_expressable - Classifier"
      (GT.prop_diploid_expressable
       :: TestClassifier -> TestClassifier -> Bool),
    testProperty "prop_diploid_readable - Classifier"
      (GT.prop_diploid_readable
       :: TestClassifier -> TestClassifier -> Bool),

    testProperty "classifySetAndTrain_label_count"
      classifySetAndTrain_label_count,
    testProperty "prop_classifier_behaves_like_sgm"
      prop_classifier_behaves_like_sgm,
    testProperty "prop_classifier_behaves_like_sgm2"
      prop_classifier_behaves_like_sgm2,
    testProperty "prop_classifier_behaves_like_sgm3"
      prop_classifier_behaves_like_sgm3,
    testProperty "prop_classifySetAndTrain_never_causes_error"
      prop_classifySetAndTrain_never_causes_error,
    -- testProperty "prop_prettyClassifierReport_never_causes_error"
    --   prop_prettyClassifierReport_never_causes_error
    testProperty "prop_imprintSet_never_causes_error"
      prop_imprintSet_never_causes_error
  ]
