------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PatternAdjusterQC
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
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.PatternAdjusterQC
  (
    test
  ) where

import qualified ALife.Creatur.Gene.Test              as GT
import           ALife.Creatur.Wain.LearningParamsQC  ()
import           ALife.Creatur.Wain.PatternAdjuster
import           ALife.Creatur.Wain.PatternQC         (TestPattern)
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      (Arbitrary, arbitrary)

type TestAdjuster = PatternAdjuster TestPattern

instance Arbitrary TestAdjuster where
  arbitrary = PatternAdjuster <$> arbitrary

test :: Test
test = testGroup "ALife.Creatur.Wain.PatternAdjusterQC"
  [
    testProperty "prop_serialize_round_trippable - PatternAdjuster"
      (GT.prop_serialize_round_trippable :: TestAdjuster -> Bool),
    testProperty "prop_genetic_round_trippable - PatternAdjuster"
      (GT.prop_genetic_round_trippable (==) :: TestAdjuster -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - PatternAdjuster"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestAdjuster -> Bool),
    testProperty "prop_diploid_identity - PatternAdjuster"
      (GT.prop_diploid_identity (==) :: TestAdjuster -> Bool),
    testProperty "prop_show_read_round_trippable - PatternAdjuster"
      (GT.prop_show_read_round_trippable (==) :: TestAdjuster -> Bool),
    testProperty "prop_diploid_expressable - PatternAdjuster"
      (GT.prop_diploid_expressable
       :: TestAdjuster -> TestAdjuster -> Bool),
    testProperty "prop_diploid_readable - PatternAdjuster"
      (GT.prop_diploid_readable
       :: TestAdjuster -> TestAdjuster -> Bool)
  ]
