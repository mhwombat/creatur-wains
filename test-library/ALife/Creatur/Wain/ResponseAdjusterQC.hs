------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ResponseAdjusterQC
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
module ALife.Creatur.Wain.ResponseAdjusterQC
  (
    test
  ) where

import qualified ALife.Creatur.Gene.Test              as GT
import           ALife.Creatur.Wain.LearningParamsQC  ()
import           ALife.Creatur.Wain.PatternAdjuster   (PatternAdjuster (..))
import           ALife.Creatur.Wain.ResponseAdjuster
import           ALife.Creatur.Wain.ResponseQC        (TestAction)
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      (Arbitrary, arbitrary)

type TestAdjuster = ResponseAdjuster TestAction

instance Arbitrary TestAdjuster where
  arbitrary = PatternAdjuster <$> arbitrary

test :: Test
test = testGroup "ALife.Creatur.Wain.ResponseAdjusterQC"
  [
    testProperty "prop_serialize_round_trippable - ResponseAdjuster"
      (GT.prop_serialize_round_trippable :: TestAdjuster -> Bool),
    testProperty "prop_genetic_round_trippable - ResponseAdjuster"
      (GT.prop_genetic_round_trippable (==) :: TestAdjuster -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - ResponseAdjuster"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> TestAdjuster -> Bool),
    testProperty "prop_diploid_identity - ResponseAdjuster"
      (GT.prop_diploid_identity (==) :: TestAdjuster -> Bool),
    testProperty "prop_show_read_round_trippable - ResponseAdjuster"
      (GT.prop_show_read_round_trippable (==) :: TestAdjuster -> Bool),
    testProperty "prop_diploid_expressable - ResponseAdjuster"
      (GT.prop_diploid_expressable
       :: TestAdjuster -> TestAdjuster -> Bool),
    testProperty "prop_diploid_readable - ResponseAdjuster"
      (GT.prop_diploid_readable
       :: TestAdjuster -> TestAdjuster -> Bool)
  ]
