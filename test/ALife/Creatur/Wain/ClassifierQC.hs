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
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.ClassifierQC
  (
    test,
    equiv
  ) where

import ALife.Creatur.Wain.Classifier
import ALife.Creatur.Wain.GeneticSOMQC (equiv)
import ALife.Creatur.Wain.TestUtils
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- prop_classification_is_consistent
--   :: Classifier TestPattern -> TestPattern -> Property
-- prop_classification_is_consistent c p = property $ bmu == bmu'
--   where (bmu, _, _, _, c') = classify c p
--         (bmu', _, _, _, _) = classify c' p

test :: Test
test = testGroup "ALife.Creatur.Wain.ClassifierQC"
  [
    testProperty "prop_serialize_round_trippable - Classifier"
      (prop_serialize_round_trippable
        :: Classifier TestPattern -> Property),
    testProperty "prop_genetic_round_trippable - Classifier"
      (prop_genetic_round_trippable equiv
        :: Classifier TestPattern -> Property),
    testProperty "prop_diploid_identity - Classifier"
      (prop_diploid_identity equiv
        :: Classifier TestPattern -> Property),
    testProperty "prop_diploid_expressable - Classifier"
      (prop_diploid_expressable
        :: Classifier TestPattern -> Classifier TestPattern -> Property),
    testProperty "prop_diploid_readable - Classifier"
      (prop_diploid_readable
        :: Classifier TestPattern -> Classifier TestPattern -> Property)
    -- testProperty "prop_classification_is_consistent"
    --   prop_classification_is_consistent
  ]
