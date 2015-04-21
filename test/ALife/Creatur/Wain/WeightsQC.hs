------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.WeightsQC
-- Copyright   :  (c) Amy de Buitléir 2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.WeightsQC
  (
    test,
    equivWeights,
    sizedArbWeights
  ) where

import ALife.Creatur.Wain.WeightsInternal
import ALife.Creatur.Wain.TestUtils
import ALife.Creatur.Wain.Util (unitInterval)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

sizedArbWeights :: Int -> Gen Weights
sizedArbWeights n = fmap makeWeights . vectorOf n $ choose unitInterval

-- sizedArbWeights :: Int -> Gen Weights
-- sizedArbWeights n = do
--   xs <- vectorOf n $ choose unitInterval
--   let s = sum xs
--   if s == 0
--     then return . Weights $ xs
--     else do
--       let ys = map (/s) xs
--       let zs = map (roundtripToWord8 unitInterval) $ ys
--       if sum zs > 1
--         then sizedArbWeights n -- try again
--         else return $ Weights zs

instance Arbitrary Weights where
  arbitrary = sized sizedArbWeights

prop_sum_of_weights_is_1 :: Weights -> Property
prop_sum_of_weights_is_1 w = not (null ws) ==> (sum ws - 1) < 1e-10
  where ws = toDoubles w

prop_weights_are_positive :: Weights -> Property
prop_weights_are_positive w = property $
  (and . map (>= 0) . toDoubles $ w)

equivWeights :: Weights -> Weights -> Bool
equivWeights x y = and $ zipWith f (toDoubles x) (toDoubles y)
  where f a b = abs (a - b) <= 1/255

test :: Test
test = testGroup "ALife.Creatur.Wain.UnitIntervalQC"
  [
    testProperty "prop_sum_of_weights_is_1"
      prop_sum_of_weights_is_1,
    testProperty "prop_weights_are_positive"
      prop_weights_are_positive,
    testProperty "prop_serialize_round_trippable - Weights"
      (prop_serialize_round_trippable :: Weights -> Property),
    testProperty "prop_genetic_round_trippable - Weights"
      (prop_genetic_round_trippable equivWeights :: Weights -> Property),
    testProperty "prop_diploid_identity - Weights"
      (prop_diploid_identity (==) :: Weights -> Property),
    testProperty "prop_diploid_expressable - Weights"
      (prop_diploid_expressable :: Weights -> Weights -> Property),
    testProperty "prop_diploid_readable - Weights"
      (prop_diploid_readable :: Weights -> Weights -> Property)
  ]
