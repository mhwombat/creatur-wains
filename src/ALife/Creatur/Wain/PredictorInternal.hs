------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PredictorInternal
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private Predictor internals.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ALife.Creatur.Wain.PredictorInternal where

import qualified ALife.Creatur.Wain.Classifier as Cl
import ALife.Creatur.Wain.GeneticSOM (GeneticSOM,
  LearningParams, Label, Tweaker(..), buildGeneticSOM, modelMap,
  trainAndClassify, hasLabel, train)
import ALife.Creatur.Wain.Response (Response(..), labels, action,
  outcomes, addToOutcomes)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, pm1Apply)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble,
  doubleToUI, normalise)
import ALife.Creatur.Wain.Weights (Weights, weightedSum, numWeights)
import Control.Lens
import Data.List (transpose, zip4)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Data.Word (Word64)
import Text.Printf (printf)

-- | A predictor predicts the outcome of a response to a scenario.
type Predictor a t = GeneticSOM (Response a) t

-- | @'buildPredictor' e n dt@ returns a Predictor, using an
--   learning function with the parameters @e@ as a learning
--   function, maximum number of models @n@,
--   and difference threshold @dt@.
buildPredictor
  :: (Eq a, Tweaker t, Pattern t ~ Response a)
    => LearningParams -> Word64 -> UIDouble -> t -> Predictor a t
buildPredictor e n dt tw = buildGeneticSOM e n dt tw

-- | @'evaluateResponse' ws actionDiff p ldss a@ predicts the outcome of
--   a proposed action,
--   where
--   @ws@ are the weights placed on the similarity of a model action to
--   the proposed action, and the model labels to the assigned labels;
--   @actionDiff@ is a function that returns the difference between two
--   actions;
--   @p@ is the predictor;
--   @ldss@ contains the labels and classifier model differences for
--   each object being responded to; and
--   @a@ is the proposed action.
--   It returns the set of predictor models paired with their
--   differences, similarities, and contributions;
--   and the predicted outcome.
evaluateResponse
  :: (Eq a, Enum a, Bounded a, Ord a)
      => UIDouble -> Weights -> (a -> a -> Cl.Difference)
        -> [PM1Double] -> Predictor a t
          -> [[(Cl.Label, Cl.Difference)]] -> a
            -> ([(Response a, Cl.Difference, UIDouble, [PM1Double])],
                 [PM1Double])
evaluateResponse dt ws actionDiff dos p ldss a
  | null rs   = ([], dos)
  | otherwise = (zip4 rs' ds ss oss', os)
  where rs = M.elems . modelMap $ p
        ls = map (fst . head) ldss
        rs' = if hasModel rs a ls
                then rs
                else (Response ls a dos):rs
        drs = map (\r -> (responseDiff ws actionDiff ldss a r, r)) rs'
        (ds, rs3) = unzip . filter (\(d, _) -> d < dt) $ drs
        -- n = length ds
        -- ss = map (\x -> doubleToUI $ (1 - uiToDouble x)/fromIntegral n) ds
        ss = normalise $ map (\x -> doubleToUI (1 - uiToDouble x)) ds
        oss = map (view outcomes) rs3
        oss' = zipWith contribution ss oss
        os = sumColumns oss'
        
hasModel :: Eq a => [Response a] -> a -> [Label] -> Bool
hasModel [] _ _ = False
hasModel (r:rs) a ls = if (_labels r == ls) && (_action r == a)
                         then True
                         else hasModel rs a ls

-- | Internal method.
--   Although this function is similar to
--   @'ALife.Creatur.Wain.SimpleResponseTweaker.responseDiff'@, it has a
--   different purpose.
--   This function is used when evaluating potential actions.
--   The other function is used to construct the SGM.
responseDiff
  :: Weights -> (a -> a -> Cl.Difference)
    -> [[(Cl.Label, Cl.Difference)]] -> a -> Response a -> Cl.Difference
responseDiff ws f ldss a r
  | numWeights ws < length ldss + 1 = error "Too few response weights"
  | otherwise                   = weightedSum ws (ad:lds)
  where lds = zipWith lookupOr1 (view labels r) ldss
        ad = f a (view action r)

-- | Internal method
lookupOr1 :: (Eq a, Num b) => a -> [(a, b)] -> b
lookupOr1 x xys = fromMaybe 1 $ lookup x xys

-- | Internal method
contribution :: UIDouble -> [PM1Double] -> [PM1Double]
contribution s os = map (pm1Apply (uiToDouble s *)) os

-- | Internal method
sumColumns :: Num a => [[a]] -> [a]
sumColumns = map sum . transpose

-- | @'imprintOrReinforce' p ls a os deltas@ teaches the predictor @p@
--   that the action @a@ is a good response when facing the scenario
--   @ls@.
--   If this response is new to the predictor, it will store it as a
--   model with outcomes @os@.
--   If the response is known, the predictor
--   will learn (reinforce) the model with @deltas@ added to its
--   outcomes.
--   Returns the label of the new or adjusted response model,
--   the new or adjusted response model itself, and the updated
--   predictor.
--   Note: The current learning rate applies when reinforcing a model,
--   so do not expect the new model to have new outcomes = previous
--   outcomes + @deltas@.
imprintOrReinforce
  :: (Eq a)
    => Predictor a t -> [Label] -> a -> [PM1Double] -> [PM1Double]
      -> (Label, Response a, Predictor a t)
imprintOrReinforce p ls a os deltas =
  if p `hasLabel` bmu
    then (bmu, rReinforced, pReinforced)
    else (bmu, rImprinted, pImprinted)
  where (bmu, pImprinted) = classifyAndMaybeCreateNewModel p rImprinted
        r = (modelMap p) M.! bmu
        rImprinted = Response ls a os
        rReinforced = deltas `addToOutcomes` r
        pReinforced = train p rReinforced

-- | Don't modify existing models, but do permit a new one to be
--   created.
classifyAndMaybeCreateNewModel
  :: (Eq a) => Predictor a t -> Response a -> (Label, Predictor a t)
classifyAndMaybeCreateNewModel p r =
  if bmu `elem` (M.keys . modelMap $ p)
    then (bmu, p)
    else (bmu, p')
  where (bmu, _, _, p') = trainAndClassify p r

decisionReport
  :: Show a
    => [(Response a, Cl.Difference, UIDouble, [PM1Double])] -> [String]
decisionReport [] = ["No response models, used default outcomes"]
decisionReport xs = concat $ map decisionReport2 xs

decisionReport2
  :: Show a
    => (Response a, Cl.Difference, UIDouble, [PM1Double]) -> [String]
decisionReport2 (r, d, s, os) =
  [ "Based on predictor response model " ++ show r,
    "diff=" ++ (printf "%.2f" .  uiToDouble $ d)
      ++ ", normalised similarity=" ++ (printf "%.2f" .  uiToDouble $ s),
    "contribution is " ++ show os ]
