------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Muser
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- The muser takes classification information (labels and
-- probabilities) about the objects in a wain's field of "view",
-- and determines the most likely scenarios that the wain could be
-- facing.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Muser
  (
    Muser(..)
  ) where

import           ALife.Creatur.Wain.GeneticSOM
    (Label)
import           ALife.Creatur.Gene.Numeric.PlusMinusOne
    (PM1Double)
import           ALife.Creatur.Wain.Probability
    (Probability)
import           ALife.Creatur.Wain.Response
    (Response (..))

class Muser m where
  type Action m
  -- | Given a set of scenarios paired with the probability that each
  --   scenario is true, returns a list of responses to consider paired
  --   with the probability that the response is based on the correct
  --   scenario.
  --   This method only generates responses; it does not evaluate how
  --   suitable the response is.
  generateResponses
    :: m
      -> [Action m]
      -> [([Label], Probability)]
      -> [(Response (Action m), Probability)]
  -- | If a wain has no model for a response it's considering, it
  --   will use these values as a prediction.
  --   Positive values make the wain optimistic and more likely to
  --   take risks. A negative value makes the wain pessimistic and
  --   risk-averse.
  defaultOutcomes :: m -> [PM1Double]
