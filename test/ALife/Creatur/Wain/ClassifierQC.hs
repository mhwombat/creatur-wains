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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.ClassifierQC
  (
    test,
    equivClassifier,
    TestThinker(..)
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Classifier
import ALife.Creatur.Wain.GeneticSOM (Thinker(..), buildGeneticSOM)
import ALife.Creatur.Wain.GeneticSOMQC (equivGSOM)
import ALife.Creatur.Wain.TestUtils
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

data TestThinker = TestThinker deriving (Eq, Show, Generic)

instance Thinker TestThinker where
  type Pattern TestThinker = TestPattern
  diff _ = testPatternDiff
  adjust _ = makeTestPatternSimilar

instance Serialize TestThinker
instance W8.Genetic TestThinker
instance Diploid TestThinker

type TestClassifier = Classifier TestPattern TestThinker

sizedArbTestClassifier :: Int -> Gen TestClassifier
sizedArbTestClassifier n = do
  e <- arbitrary
  xs <- vectorOf (n+1) arbitrary
  return $ buildGeneticSOM e TestThinker xs

instance Arbitrary TestClassifier where
  arbitrary = sized sizedArbTestClassifier

-- prop_classification_is_consistent
--   :: Classifier TestPattern -> TestPattern -> Property
-- prop_classification_is_consistent c p = property $ bmu == bmu'
--   where (bmu, _, _, _, c') = classify c p
--         (bmu', _, _, _, _) = classify c' p

equivClassifier :: TestClassifier -> TestClassifier -> Bool
equivClassifier = equivGSOM (==) (==)

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
        :: TestClassifier -> TestClassifier -> Property)
    -- testProperty "prop_classification_is_consistent"
    --   prop_classification_is_consistent
  ]
