------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.SimpleResponseTweakerQC
-- Copyright   :  (c) Amy de Buitléir 2013-2018
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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.SimpleResponseTweakerQC
  (
    test
  ) where

import ALife.Creatur.Wain.ResponseQC (TestResponse, TestAction)
import ALife.Creatur.Wain.SimpleResponseTweaker
import ALife.Creatur.Wain.Statistics (Statistical(..))
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.UnitIntervalQC ()
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Statistical (ResponseTweaker TestAction) where
  stats _ = []

-- prop_responseDiff_can_be_1 :: Weights -> Property
-- prop_responseDiff_can_be_1 w = not (null ws) ==> abs (x - 1) < 1e-8
--   where x = responseDiff w (Response [0] [0] (Condition 0 0 0))
--               (Response [1] [1] (Condition 1 1 1))
--         ws = toDoubles w

prop_responseDiff_can_be_0 :: TestResponse -> Property
prop_responseDiff_can_be_0 r = property $ abs (x - 0) < 1e-8
  where x = responseDiff r r

prop_responseDiff_in_range
  :: TestResponse -> TestResponse -> Property
prop_responseDiff_in_range a b = property $ 0 <= x && x <= 1
  where x = responseDiff a b

prop_makeResponseSimilar_works
  :: TestResponse -> UIDouble -> TestResponse -> Property
prop_makeResponseSimilar_works
  = prop_makeSimilar_works responseDiff makeResponseSimilar


test :: Test
test = testGroup "ALife.Creatur.Wain.SimpleResponseTweakerQC"
  [
    -- testProperty "prop_responseDiff_can_be_1"
    --   prop_responseDiff_can_be_1,
    testProperty "prop_responseDiff_can_be_0"
      prop_responseDiff_can_be_0,
    testProperty "prop_responseDiff_in_range"
      prop_responseDiff_in_range,
    testProperty "prop_makeResponseSimilar_works"
      prop_makeResponseSimilar_works
  ]
