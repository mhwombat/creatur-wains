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
    TestThinker(..)
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Classifier
import ALife.Creatur.Wain.GeneticSOM (Thinker(..), buildGeneticSOM,
  models)
import ALife.Creatur.Wain.GeneticSOMQC (equivGSOM)
import ALife.Creatur.Wain.TestUtils
import Control.DeepSeq (NFData)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding (classify)

data TestThinker = TestThinker
  deriving (Eq, Show, Generic, Serialize, W8.Genetic, Diploid, NFData)

instance Thinker TestThinker where
  type Pattern TestThinker = TestPattern
  diff _ = testPatternDiff
  adjust _ = makeTestPatternSimilar

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

prop_classify_only_tweaks_bmu
  :: TestClassifier -> TestPattern -> Property
prop_classify_only_tweaks_bmu c p = property $
  (as ++ bs) == (as' ++ bs')
  where (as, _:bs) = splitAt bmu $ models c
        (as', _:bs') = splitAt bmu $ models c'
        bmu = fst . minimumBy (comparing snd) $ zip [0..] diffs
        (_, diffs, _, c') = classify c p

prop_classification_stabilises
  :: TestClassifier -> [TestPattern] -> Property
prop_classification_stabilises c ps = not (null ps) ==> k2 == k1
  where (_, _, _, cStable) = classifyAll c . concat . replicate 10 $ ps
        (k1, _, _, cStable2) = classify cStable (head ps)
        (_, _, _, cStable3) = classifyAll cStable2 ps
        (k2, _, _, _) = classify cStable3 (head ps)

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
        :: TestClassifier -> TestClassifier -> Property),
    testProperty "prop_classify_only_tweaks_bmu"
      prop_classify_only_tweaks_bmu,
    testProperty "prop_classification_stabilises"
      prop_classification_stabilises
    -- testProperty "prop_classification_is_consistent"
    --   prop_classification_is_consistent
  ]
