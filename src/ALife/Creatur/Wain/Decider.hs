------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Decider
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A decision-maker based on a Kohonen Self-organising Map (SOM).
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module ALife.Creatur.Wain.Decider
  (
    Label,
    Decider,
    DeciderTweaker(..),
    buildDecider,
    predict, -- exported for testing only
    predictAll,
    imprint,
    deciderQuality
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.GeneticSOM (GeneticSOM, ExponentialParams(..),
  Label, Tweaker(..), buildGeneticSOM, tweaker, discrimination,
  classify, train, modelMap, counterMap)
import ALife.Creatur.Wain.Response (Response(..), outcome,
  diffIgnoringOutcome, similarityIgnoringOutcome, makeResponseSimilar,
  action)
import ALife.Creatur.Wain.Scenario (Scenario)
import ALife.Creatur.Wain.PlusMinusOne (pm1Apply)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble)
import ALife.Creatur.Wain.Weights (Weights)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Function (on)
import Data.List (groupBy, sortBy, foldl')
import qualified Data.Map.Strict as M
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- | @'DeciderTweaker' cw sw rw@ constructs an object which is
--   responsible for comparing and adjusting response patterns.
--   The parameter @cw@ determines the relative weight to assign to
--   differences in energy, passion, and whether or not there is a
--   litter when comparing two patterns.
--   The parameter @sw@ determines the relative weight to assign to
--   differences between each corresponding pair of objects represented
--   by the scenario when comparing two patterns.
--   The parameter @rw@ determines the relative weight to assign to
--   differences in the scenarios and the outcomes when comparing two
--   patterns.
--
--   NOTE: In order to declare an instance of @Tweaker@, we have to
--   reference the action type @a@. As a result, @DeciderTweaker@ has
--   to have a type parameter @a@, even though it is not used.
data DeciderTweaker a = DeciderTweaker Weights Weights Weights
  deriving (Eq, Show, Generic, NFData, Serialize, Genetic, Diploid)

instance (Eq a) => Tweaker (DeciderTweaker a) where
  type Pattern (DeciderTweaker a) = Response a
  diff (DeciderTweaker cw sw rw) = diffIgnoringOutcome cw sw rw
  adjust _ = makeResponseSimilar

type Decider a = GeneticSOM (Response a) (DeciderTweaker a)

-- | @'buildDecider' e n dt cw sw rw@ returns a decider, using an
--   exponential function with the parameters @e@ as a learning
--   function, maximum number of models @n@, difference threshold @dt@,
--   and "tweaker" defined by the weights @cw@, @sw@, and @rw@.
buildDecider
  :: Eq a
    => ExponentialParams -> Int -> UIDouble -> Weights -> Weights
      -> Weights -> Decider a
buildDecider e n dt cw sw rw
  = buildGeneticSOM e n dt (DeciderTweaker cw sw rw)

-- | Predicts the outcome of a response based on the decider models,
--   and updates the outcome field in that response.
--   If the decider has no model for this response, it creates a new
--   model and returns the response unmodified.
predict :: (Eq a) => Decider a -> Response a -> (Response a, Label, Decider a)
predict d r = (r', bmu, d')
  where (bmu, _, _, d') = classify d r
        model = modelMap d' M.! bmu
        rawOutcome = _outcome model
        (DeciderTweaker cw sw rw)  = view tweaker d
        adjustment = uiToDouble
          $ similarityIgnoringOutcome cw sw rw model r
        adjustedOutcome = pm1Apply (adjustment *) rawOutcome
        r' = set outcome adjustedOutcome r

-- | Predicts the outcomes of a set of responses based on the decider
--   models, and updates the outcome field in each response.
predictAll
  :: Eq a
    => Decider a -> [Response a] -> ([(Response a, Label)], Decider a)
predictAll d rs = foldl' predictOne ([], d) rs

predictOne
  :: Eq a
    => ([(Response a, Label)], Decider a) -> Response a
      -> ([(Response a, Label)], Decider a)
predictOne (rls, d) r = ((r', l):rls, d')
  where (r', l, d') = predict d r

-- | Teaches a response to the decider (teaches it that the response
--   increases happiness by 1).
imprint :: (Eq a) => Decider a -> Scenario -> a -> Decider a
imprint d s a = d'
  where r = Response s a 1
        d' = train d r

deciderQuality :: (Eq a, Ord a) => Decider a -> Int
deciderQuality d = discrimination actionCounts
  where modelCounts = M.elems . counterMap $ d
        actions = map (view action) . M.elems . modelMap $ d
        actionCounts = map (sum . map snd)
                         . groupBy ((==) `on` fst)
                         . sortBy (compare `on` fst)
                           $ zip actions modelCounts
