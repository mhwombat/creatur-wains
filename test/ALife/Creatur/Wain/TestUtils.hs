------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.TestUtils
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck test utilities.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.TestUtils
  (
    TestPattern(..),
    arb8BitDouble,
    arb8BitInt,
    prop_serialize_round_trippable,
    prop_genetic_round_trippable,
    prop_diploid_identity,
    prop_diploid_expressable,
    prop_diploid_readable,
    prop_show_read_round_trippable,
    randomTestPattern
--    test
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Util (fromEither)
import ALife.Creatur.Wain.Util (scaleFromWord8, scaleWord8ToInt,
  forceToWord8)
import Control.Applicative ((<$>))
import Control.Monad.Random (Rand, RandomGen, getRandom)
import Data.Datamining.Pattern (Pattern(..), Metric, adjustNum)
import Data.Serialize (Serialize, encode, decode)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Test.QuickCheck

-- instance (Floating a, Fractional a, Ord a, Eq a) => Pattern [a] where
--   type Metric [a] = a
--   difference = euclideanDistanceSquared
--   makeSimilar = adjustVector

arb8BitDouble :: (Double, Double) -> Gen Double
arb8BitDouble interval = do 
  x <- arbitrary :: Gen Word8
  return $ scaleFromWord8 interval x
  
arb8BitInt :: (Int, Int) -> Gen Int
arb8BitInt interval = do 
  x <- arbitrary :: Gen Word8
  return $ scaleWord8ToInt interval x

prop_serialize_round_trippable :: (Eq a, Serialize a) => a -> Property
prop_serialize_round_trippable x = property $ x' == Right x
  where bs = encode x
        x' = decode bs

prop_genetic_round_trippable :: (Eq g, W8.Genetic g, Show g) =>
  (g -> g -> Bool) -> g -> Property
prop_genetic_round_trippable eq g = property $ g' `eq` g
  where x = W8.write g
        g' = fromEither (error "read returned Nothing") . W8.read $ x

prop_diploid_identity :: Diploid g => (g -> g -> Bool) -> g -> Property
prop_diploid_identity eq g = property $ express g g `eq` g

prop_show_read_round_trippable
  :: (Read a, Show a) => (a -> a -> Bool) -> a -> Property
prop_show_read_round_trippable eq x
  = property $ (read . show $ x) `eq` x

prop_diploid_expressable :: (Diploid g, W8.Genetic g) => g -> g -> Property
prop_diploid_expressable a b = property $ seq (express a b) True

prop_diploid_readable :: (Diploid g, W8.Genetic g) => g -> g -> Property
prop_diploid_readable a b = property $ seq (c `asTypeOf` a) True
  where ga = W8.write a
        gb = W8.write b
        (Right c) = W8.runDiploidReader W8.getAndExpress (ga, gb)

data TestPattern = TestPattern Word8
  deriving (Show, Eq, Generic)

instance Serialize TestPattern
instance W8.Genetic TestPattern
instance Diploid TestPattern

instance Arbitrary TestPattern where
  arbitrary = TestPattern <$> arbitrary

instance Pattern TestPattern where
  type Metric TestPattern = Double
  difference (TestPattern x) (TestPattern y)
    = abs (fromIntegral x - fromIntegral y) / 255
  makeSimilar (TestPattern target) r (TestPattern x)
    = TestPattern (forceToWord8 x'')
    where t' = fromIntegral target :: Double
          x' = fromIntegral x :: Double
          x'' = adjustNum t' r x'

randomTestPattern :: RandomGen r => Rand r TestPattern
randomTestPattern = fmap (TestPattern) getRandom
