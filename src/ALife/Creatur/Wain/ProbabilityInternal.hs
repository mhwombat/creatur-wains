------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ProbabilityInternal
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private Probability internals.
-- Most developers should use Probability instead.
-- This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ALife.Creatur.Wain.ProbabilityInternal where

import           ALife.Creatur.Wain.GeneticSOM
    (Difference, Label)
import           ALife.Creatur.Wain.UnitInterval
    (UIDouble, doubleToUI, uiToDouble)
import           Data.Word
    (Word64)
import           Text.Printf
    (printf)

-- | Estimated probability that a set of labels is accurate.
type Probability = UIDouble

-- | Returns a set of hypotheses about the scenario the wain is facing,
--   paired with the estimated probability that each hypothesis is true.
hypothesise
  :: Word64 -> [[(Label, Difference)]] -> [([Label], Probability)]
hypothesise x = map jointProbability . permute . diffsToProbs x

-- | Internal method.
permute :: [[a]] -> [[a]]
permute (xs:[])  = [ [y] | y <- xs ]
permute (xs:xss) = [ y:ys | y <- xs, ys <- permute xss]
permute []       = []

-- | Given a list of labels for each object the wain is seeing, paired
--   with the probability that each label is accurate, returns
--   the probability that all labels are accurate.
jointProbability :: [(Label, Probability)] -> ([Label], Probability)
jointProbability xs = (ls, prob)
  where ls = map fst xs
        prob = product . map snd $ xs

-- | Make the sum of the probabilities be one.
normalise :: (Eq a, Fractional a) => [a] -> [a]
normalise xs
  | x == 0     = map (const (1/fromIntegral (length xs))) xs
  | otherwise = map (/x) xs
  where x = sum xs

-- | Given the signature for one input pattern, calculate the
--   probability that the pattern should match each classifier model.
diffsToProbs :: Word64 -> [[(Label, Difference)]] -> [[(Label, Probability)]]
diffsToProbs x = map (diffsToProbs2 x)

-- | Given the signature for one input pattern, calculate the
--   probability that the pattern should match each classifier model.
diffsToProbs2 :: Word64 -> [(Label, Difference)] -> [(Label, Probability)]
diffsToProbs2 x lds = zip ls ps
  where ls = map fst lds
        ds = map snd lds
        ps = diffsToProbs1 x ds

-- | Given a set of differences between an input pattern and a model,
--   calculate the probability that the pattern should match each model.
diffsToProbs1 :: Word64 -> [Difference] -> [Probability]
diffsToProbs1 x ds = map doubleToUI . normalise . map uiToDouble $ ps
  where ps = map (diffToProb x) ds'
        ds' = map doubleToUI . normalise $ map uiToDouble ds

-- | @'diffToProb' x d@ converts a difference metric @d@
--   into an estimated probability that the classification is "sound".
--   The parameter @x@ controls how quickly the probability falls as
--   the difference increases.
diffToProb :: Word64 -> UIDouble -> UIDouble
diffToProb x d = doubleToUI $ 1 - exp(1 - 1/(1 - (1 - d')^x'))
  where x' = fromIntegral x :: Integer
        d' = uiToDouble d

prettyProbability :: Probability -> String
prettyProbability p = printf "%.3f" (uiToDouble p * 100) ++ "%"
