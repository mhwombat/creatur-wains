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
    TestClassifier
  ) where

import qualified ALife.Creatur.Gene.Test               as GT
import           ALife.Creatur.Wain.ClassifierInternal
import qualified ALife.Creatur.Wain.GeneticSOM         as GSOM
import           ALife.Creatur.Wain.GeneticSOMQC       (TestGSOM)
import           ALife.Creatur.Wain.PatternQC          (TestPattern)
import           Control.DeepSeq                       (deepseq)
import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2  (testProperty)

type TestClassifier = TestGSOM

classifySetAndTrain_label_count
  :: TestClassifier -> [TestPattern] -> Bool
classifySetAndTrain_label_count c ps = length xs == length ps
  where (r, _) = classifySetAndTrain c ps
        xs = bmus r

prop_classifySetAndTrain_never_causes_error
  :: TestClassifier -> [TestPattern] -> Bool
prop_classifySetAndTrain_never_causes_error c ps
  = deepseq (classifySetAndTrain c ps) True

-- prop_prettyClassifierReport_never_causes_error
--   :: TestClassifier -> [TestPattern] -> Bool
-- prop_prettyClassifierReport_never_causes_error c ps
--   = deepseq x True
--   where (r, _) = classifySetAndTrain c ps
--         x = prettyClassifierReport r

prop_imprintSet_never_causes_error
  :: TestClassifier -> [(GSOM.Label, TestPattern)] -> Bool
prop_imprintSet_never_causes_error c lps
  = deepseq (imprintSet c lps) True

test :: Test
test = testGroup "ALife.Creatur.Wain.ClassifierQC"
  [
    testProperty "prop_serialize_round_trippable - Classifier"
      (GT.prop_serialize_round_trippable :: TestClassifier -> Bool),
    testProperty "prop_genetic_round_trippable - Classifier"
      (GT.prop_genetic_round_trippable (==) :: TestClassifier -> Bool),
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
    testProperty "prop_classifySetAndTrain_never_causes_error"
      prop_classifySetAndTrain_never_causes_error,
    -- testProperty "prop_prettyClassifierReport_never_causes_error"
    --   prop_prettyClassifierReport_never_causes_error
    testProperty "prop_imprintSet_never_causes_error"
      prop_imprintSet_never_causes_error
  ]
