------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ProbabilityInternal
-- Copyright   :  (c) Amy de Buitléir 2012-2015
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ALife.Creatur.Wain.ProbabilityInternal where

import ALife.Creatur.Wain.GeneticSOM (Difference, Label)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI,
  uiToDouble)

-- | Estimated probability that a set of labels is accurate.
type Probability = UIDouble

-- | Returns a set of hypotheses about the scenario the wain is facing,
--   paired with the estimated probability that each hypothesis is true.
hypothesise
  :: [[(Label, Difference)]] -> [([Label], Probability)]
hypothesise = map jointProbability . permute . diffsToProbs

-- | Internal method.
permute :: [[a]] -> [[a]]
permute (xs:[]) = [ [y] | y <- xs ]
permute (xs:xss) = [ y:ys | y <- xs, ys <- permute xss]
permute [] = []

-- | Given a list of labels for each object the wain is seeing, paired
--   with the probability that each label is accurate, returns
--   the probability that all labels are accurate.
jointProbability :: [(Label, Probability)] -> ([Label], Probability)
jointProbability xs = (ls, prob)
  where ls = map fst xs
        prob = product . map snd $ xs

-- | Make the sum of the probabilities be one.
normalise :: Fractional a => [a] -> [a]
normalise xs = map (/x) xs
  where x = sum xs

-- | Given the signature for one input pattern, calculate the
--   probability that the pattern should match each classifier model.
diffsToProbs :: [[(Label, Difference)]] -> [[(Label, Probability)]]
diffsToProbs = map diffsToProbs2

-- | Given the signature for one input pattern, calculate the
--   probability that the pattern should match each classifier model.
diffsToProbs2 :: [(Label, Difference)] -> [(Label, Probability)]
diffsToProbs2 lds = zip ls ps
  where ls = map fst lds
        ds = map snd lds
        ps = diffsToProbs1 ds

-- | Given a set of differences between an input pattern and a model,
--   calculate the probability that the pattern should match each model.
diffsToProbs1 :: [Difference] -> [Probability]
diffsToProbs1 ds = map doubleToUI . normalise . map uiToDouble $ ps
  where ps = map (\d -> 1 - d) ds
