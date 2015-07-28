------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ClassifierQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.ClassifierQC
  (
    test,
    equivClassifier,
    TestTweaker(..)
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Classifier
import ALife.Creatur.Wain.GeneticSOMInternal (Tweaker(..))
import ALife.Creatur.Wain.GeneticSOMQC (equivGSOM, sizedArbGeneticSOM)
import ALife.Creatur.Wain.TestUtils
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding (classify, maxSize)

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

equivClassifier :: TestClassifier -> TestClassifier -> Bool
equivClassifier = equivGSOM (==)

classifySetAndTrain_label_count
  :: TestClassifier -> [TestPattern] -> Property
classifySetAndTrain_label_count c ps = property $ length xs == length ps
  where (xs, _) = classifySetAndTrain c ps

test :: Test
test = testGroup "ALife.Creatur.Wain.ClassifierQC"
  [
    testProperty "prop_serialize_round_trippable - Classifier"
      (prop_serialize_round_trippable
        :: TestClassifier -> Property),
    testProperty "prop_genetic_round_trippable - Classifier"
      (prop_genetic_round_trippable equivClassifier
        :: TestClassifier -> Property),
    testProperty "prop_diploid_identity - Classifier"
      (prop_diploid_identity (==)
        :: TestClassifier -> Property),
    testProperty "prop_diploid_expressable - Classifier"
      (prop_diploid_expressable
        :: TestClassifier -> TestClassifier -> Property),
    testProperty "prop_diploid_readable - Classifier"
      (prop_diploid_readable
        :: TestClassifier -> TestClassifier -> Property),
    testProperty "classifySetAndTrain_label_count"
      classifySetAndTrain_label_count
  ]
