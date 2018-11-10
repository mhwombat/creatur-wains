------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ClassifierQC
-- Copyright   :  (c) Amy de Buitléir 2013-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.ClassifierQC
  (
    test,
    equivClassifier,
    TestTweaker(..),
    TestClassifier
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8       as W8
import           ALife.Creatur.Genetics.Diploid
    (Diploid)
import           ALife.Creatur.Wain.Classifier
import           ALife.Creatur.Wain.GeneticSOMInternal
    (Tweaker (..), modelMap, patternMap)
import           ALife.Creatur.Wain.GeneticSOMQC
    (equivGSOM, sizedArbGeneticSOM)
import           ALife.Creatur.Wain.Statistics
    (Statistical (..))
import           ALife.Creatur.Wain.TestUtils
import           Control.DeepSeq
    (NFData, deepseq)
import           Control.Lens
import qualified Data.Datamining.Clustering.SGMInternal as SOM
import qualified Data.Map.Lazy                          as M
import           Data.Serialize
    (Serialize)
import           GHC.Generics
    (Generic)
import           Test.Framework
    (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2
    (testProperty)
import           Test.QuickCheck                        hiding
    (classify, maxSize)

data TestTweaker = TestTweaker
  deriving (Eq, Show, Generic, Serialize, W8.Genetic, Diploid, NFData)

instance Tweaker TestTweaker where
  type Pattern TestTweaker = TestPattern
  diff _ = testPatternDiff
  adjust _ = makeTestPatternSimilar

instance Arbitrary TestTweaker where
  arbitrary = return TestTweaker

type TestClassifier = Classifier TestPattern TestTweaker

instance Arbitrary TestClassifier where
  arbitrary = sized (sizedArbGeneticSOM arbitrary)

instance Statistical TestTweaker where
  stats _ = []

equivClassifier :: TestClassifier -> TestClassifier -> Bool
equivClassifier = equivGSOM (==)

classifySetAndTrain_label_count
  :: TestClassifier -> [TestPattern] -> Property
classifySetAndTrain_label_count c ps = property $ length xs == length ps
  where (r, _) = classifySetAndTrain c ps
        xs = bmus r

prop_classifier_behaves_like_sgm
  :: TestClassifier -> TestPattern -> Property
prop_classifier_behaves_like_sgm c p =
  property $ cModels == sModels
  where s = view patternMap c
        (_, c') = classifySetAndTrain c [p]
        (_, _, _, s') = SOM.trainAndClassify s p
        cModels = M.elems . modelMap $ c'
        sModels = M.elems . SOM.modelMap $ s'

prop_classifier_behaves_like_sgm2
  :: TestClassifier -> TestPattern -> Property
prop_classifier_behaves_like_sgm2 c p =
  property $ cLds == [sLds]
  where s = view patternMap c
        (report, _) = classifySetAndTrain c [p]
        cLds = bmus report
        (sLds, _, _, _) = SOM.trainAndClassify s p

prop_classifier_behaves_like_sgm3
  :: TestClassifier -> [TestPattern] -> Property
prop_classifier_behaves_like_sgm3 c ps =
  property $ cModels == sModels
  where s = view patternMap c
        (_, c') = classifySetAndTrain c ps
        s' = SOM.trainBatch s ps
        cModels = M.elems . modelMap $ c'
        sModels = M.elems . SOM.modelMap $ s'

prop_classifySetAndTrain_never_causes_error
  :: TestClassifier -> [TestPattern] -> Property
prop_classifySetAndTrain_never_causes_error c ps
  = property $ deepseq x True
  where x = classifySetAndTrain c ps

-- prop_prettyClassifierReport_never_causes_error
--   :: TestClassifier -> [TestPattern] -> Property
-- prop_prettyClassifierReport_never_causes_error c ps
--   = property $ deepseq x True
--   where (r, _) = classifySetAndTrain c ps
--         x = prettyClassifierReport r

test :: Test
test = testGroup "ALife.Creatur.Wain.ClassifierQC"
  [
    testProperty "prop_serialize_round_trippable - Classifier"
      (prop_serialize_round_trippable :: TestClassifier -> Property),
    testProperty "prop_genetic_round_trippable - Classifier"
      (prop_genetic_round_trippable equivClassifier
       :: TestClassifier -> Property),
    -- testProperty "prop_genetic_round_trippable2 - Classifier"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestClassifier -> Property),
    testProperty "prop_diploid_identity - Classifier"
      (prop_diploid_identity (==) :: TestClassifier -> Property),
    -- testProperty "prop_show_read_round_trippable - Classifier"
    --   (prop_show_read_round_trippable (==) :: TestClassifier -> Property),
    testProperty "prop_diploid_expressable - Classifier"
      (prop_diploid_expressable
       :: TestClassifier -> TestClassifier -> Property),
    testProperty "prop_diploid_readable - Classifier"
      (prop_diploid_readable
       :: TestClassifier -> TestClassifier -> Property),

    testProperty "classifySetAndTrain_label_count"
      classifySetAndTrain_label_count,
    testProperty "prop_classifier_behaves_like_sgm"
      prop_classifier_behaves_like_sgm,
    testProperty "prop_classifier_behaves_like_sgm2"
      prop_classifier_behaves_like_sgm2,
    testProperty "prop_classifier_behaves_like_sgm3"
      prop_classifier_behaves_like_sgm3,
    testProperty "prop_classifySetAndTrain_never_causes_error"
      prop_classifySetAndTrain_never_causes_error
    -- testProperty "prop_prettyClassifierReport_never_causes_error"
    --   prop_prettyClassifierReport_never_causes_error
  ]
