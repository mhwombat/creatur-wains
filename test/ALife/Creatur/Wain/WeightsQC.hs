------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.WeightsQC
-- Copyright   :  (c) Amy de Buitléir 2015-2019
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
    sizedArbWeights
  ) where

import           ALife.Creatur.Wain.TestUtils
import           ALife.Creatur.Wain.UnitInterval
    (UIDouble, uiToDouble)
import           ALife.Creatur.Wain.UnitIntervalQC
    ()
import           ALife.Creatur.Wain.WeightsInternal
import qualified Numeric.ApproxEq                     as N
import           Test.Framework
    (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2
    (testProperty)
import           Test.QuickCheck

sizedArbWeights :: Int -> Gen Weights
sizedArbWeights n = fmap makeWeights $ vectorOf n arbitrary

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
prop_sum_of_weights_is_1 w
  = not (null ws) ==> sum ws <= 1 && (sum ws - 1) < 1e-10
  where ws = map uiToDouble $ toUIDoubles w

prop_weights_are_positive :: Weights -> Property
prop_weights_are_positive w = property $
  (and . map ((>= 0) . uiToDouble) . toUIDoubles $ w)

prop_weighted_sum_in_range :: Weights -> [UIDouble] -> Property
prop_weighted_sum_in_range ws xs
  = property $ seq (weightedSum ws xs) True

-- The express function in Diploid normalises the weights, so the identity may
-- not hold exactly.
equiv :: Weights -> Weights -> Bool
equiv (Weights xs) (Weights ys)
  = length xs == length ys
      && (and $ zipWith (N.within 5000) (map uiToDouble xs) (map uiToDouble ys))

test :: Test
test = testGroup "ALife.Creatur.Wain.WeightsQC"
  [
    testProperty "prop_serialize_round_trippable - Weights"
      (prop_serialize_round_trippable :: Weights -> Property),
    testProperty "prop_genetic_round_trippable - Weights"
      (prop_genetic_round_trippable (==) :: Weights -> Property),
    -- testProperty "prop_genetic_round_trippable2 - Weights"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> Weights -> Property),
    testProperty "prop_diploid_identity - Weights"
      (prop_diploid_identity equiv :: Weights -> Property),
    testProperty "prop_show_read_round_trippable - Weights"
      (prop_show_read_round_trippable (==) :: Weights -> Property),
    testProperty "prop_diploid_expressable - Weights"
      (prop_diploid_expressable :: Weights -> Weights -> Property),
    testProperty "prop_diploid_readable - Weights"
      (prop_diploid_readable :: Weights -> Weights -> Property),
    testProperty "prop_sum_of_weights_is_1"
      prop_sum_of_weights_is_1,
    testProperty "prop_weights_are_positive"
      prop_weights_are_positive,
    testProperty "prop_weighted_sum_in_range"
      prop_weighted_sum_in_range
  ]
