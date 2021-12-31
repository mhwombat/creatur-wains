------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PredictorInternal
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private Predictor internals.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ALife.Creatur.Wain.PredictorInternal where

import qualified ALife.Creatur.Gene.Numeric.PlusMinusOne as PM1
import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import qualified ALife.Creatur.Wain.GeneticSOM           as GSOM
import           ALife.Creatur.Wain.Pretty               (Pretty (..))
import           ALife.Creatur.Wain.Probability          (prettyProbability)
import           ALife.Creatur.Wain.Response             (Response (..),
                                                          addToOutcomes, labels,
                                                          outcomes)
import           Control.DeepSeq                         (NFData)
import qualified Data.Datamining.Clustering.SGM4         as SOM
import qualified Data.Datamining.Pattern.List            as L
import           Data.List                               (nub, (\\))
import qualified Data.Map.Strict                         as M
import           Data.Word                               (Word32)
import           GHC.Generics                            (Generic)
import           Text.Printf                             (printf)

-- | A predictor predicts the outcome of a response to a scenario.
type Predictor t a = GSOM.GeneticSOM t (Response a)

-- | Information about how a predictor generated a prediction
data PredictionDetail a
  = PredictionDetail
      {
        -- | The response, updated with the predicted outcomes
        pResponse    :: Response a,
        -- | The probability of the scenario on which the prediction
        --   is based
        pProb        :: UI.Double,
        -- | The label of the node that best matches the input
        pBmu         :: Maybe GSOM.Label,
        -- | The BMU's model
        pBmuModel    :: Response a,
        -- | A measure of how novel the response pattern was to the wain.
        --   It is the difference between the input pattern and the
        --   closest model prior to any training or addition of models.
        pNovelty     :: UI.Double,
        -- | A measure of how novel the response pattern was to the wain,
        --   adjusted based on the age of the wain.
        pAdjNovelty  :: Int,
        -- | The adjusted probability based on how well the model
        --   matches the proposed response
        pAdjustment  :: UI.Double,
        -- | The unadjusted outcomes from the model
        pRawOutcomes :: [PM1.Double],
        -- | Even more details about the prediction
        pDetails     :: M.Map GSOM.Label (Response a, UI.Double)
      } deriving (Generic, Read, Show, NFData)

instance (Pretty a) => Pretty (PredictionDetail a) where
  pretty r = pretty (pResponse r)
               ++ " prob: " ++ prettyProbability (pProb r)
               ++ bmuMsg
               ++ " raw outcomes: "
               ++ unwords (map (printf "%.3f" . PM1.wide) os)
    where os = pRawOutcomes r
          bmuMsg = case pBmu r of
                     Just bmu -> " based on model " ++ show bmu
                     Nothing  -> " based on default outcomes"

-- | @'predict' p r k@ uses the predictor @p@ to predict the outcome
--   of the response @r@ to the onput @p@, given the probability @k@
--   that the scenario associated with the response is the one
--   we're actually facing.
--   Returns a detailed report including the prediction and
--   information about how the prediction was generated.
predict
  :: (SOM.Adjuster t, Eq a, SOM.PatternType t ~ Response a,
     SOM.MetricType t ~ UI.Double, SOM.TimeType t ~ Word32)
  => Predictor t a -> Response a -> UI.Double -> PredictionDetail a
predict p r prob = PredictionDetail
                     {
                       pResponse = r',
                       pProb = prob,
                       pBmu = bmu',
                       pBmuModel = model,
                       pNovelty = novelty,
                       pAdjNovelty = GSOM.cAdjNovelty report,
                       pAdjustment = adjustment,
                       pRawOutcomes = rawOutcomes,
                       pDetails = GSOM.cDetails report
                     }
  where (report, _) = GSOM.trainAndClassify p r
        bmu = GSOM.cBmu report
        novelty = GSOM.cNovelty report
        -- If the predictor already contained a suitable model, then
        -- use it. Otherwise, use r (which has default outcomes).
        (bmu', model) | p `GSOM.hasLabel` bmu
                          = (Just bmu, p `SOM.modelAt` bmu)
                      | otherwise = (Nothing, r)
        rawOutcomes = outcomes model
        -- Adjust the outcome based on how well the model
        -- matches the proposed response. Specifically, we're comparing
        -- the classifications and the conditions, in the model and the
        -- proposed response.
        adjustment = prob * (1 - novelty)
        -- Adjust from zero because, depending on the similarity
        -- between the true scenario and the model, the action may have
        -- less of an effect (positive or negative) than predicted by
        -- the model.
        zeroes = map (const 0) rawOutcomes
        adjustedOutcomes
          = L.makeSimilar PM1.makeDoubleSimilar rawOutcomes adjustment zeroes
        r' = r { outcomes=adjustedOutcomes }

-- | Information about how a predictor learned.
data LearningReport a
  = LearningReport
      {
        -- | The current learning rate for the predictor
        lLearningRate :: UI.Double,
        -- | Is the pattern new (imprinted) or old (reinforced)
        lNew          :: Bool,
        -- | The response that was learned
        lResponse     :: Response a,
        -- | The label of the predictor node that best matches the input
        lBmu          :: GSOM.Label,
        -- | A measure of how novel the input pattern was to the wain.
        lNovelty      :: UI.Double,
        -- | A measure of how novel the input pattern was to the wain,
        --   adjusted based on the age of the wain.
        lAdjNovelty   :: Int,
        -- | Even more details about the classification
        lDetails      :: M.Map GSOM.Label (Response a, UI.Double)
      } deriving (Generic, Read, Show, NFData)

prettyLearningReport
  :: Pretty a
  => LearningReport a -> [String]
prettyLearningReport r =
  [
    "predictor learning rate: " ++ pretty (lLearningRate r),
    msg ++ pretty (lResponse r),
    "predictor BMU: " ++ show (lBmu r)
      ++ " difference: " ++ pretty (lNovelty r)
      ++ " novelty: " ++ pretty (lAdjNovelty r),
    "learning details (label, model, diff):"
  ] ++ GSOM.prettyClassificationMoreDetail (lDetails r)
  where msg = if lNew r
                then "Imprinted new response model: "
                else "Reinforced existing response model: "

-- | @'imprintOrReinforce' d ls a os deltas@ teaches the predictor that
--   the action @a@ is a good response when facing the scenario @ls@.
--   If this response is new to the predictor, it will store it as a new
--   model with outcomes @os@. If the response is known, the predictor
--   will learn (reinforce) the model with @deltas@ added to its
--   outcomes.
--   Returns the label of the new or adjusted response model,
--   the new or adjusted response model itself,
--   and the updated predictor.
--   Note: The current learning rate applies when reinforcing a model,
--   so do not expect the new model to have new outcomes = previous
--   outcomes + @deltas@.
imprintOrReinforce
  :: (SOM.Adjuster t, Eq a, SOM.PatternType t ~ Response a,
     SOM.MetricType t ~ UI.Double, SOM.TimeType t ~ Word32)
  => Predictor t a -> [GSOM.Label] -> a -> [PM1.Double] -> [PM1.Double]
  -> (LearningReport a, Predictor t a)
imprintOrReinforce d ls a os deltas =
  if lNew reportI
    then (reportI, dI)
    else (reportR, dR)
  where (reportI, dI) = learn d rI -- imprinting new model
        (reportR, dR) = learn d rR -- reinforcing existing model
        r = SOM.modelMap d M.! bmuI
        rI = Response ls a os
        rR = deltas `addToOutcomes` r
        bmuI = lBmu reportI

-- | @'learn' p r@ teaches the response @r@ to the predictor @p@.
learn
  :: (SOM.Adjuster t, Eq a, SOM.PatternType t ~ Response a,
     SOM.MetricType t ~ UI.Double, SOM.TimeType t ~ Word32)
  => Predictor t a -> Response a
  -> (LearningReport a, Predictor t a)
learn d r = (report', d')
  where (report, d') = GSOM.trainAndClassify d r
        bmu = GSOM.cBmu report
        existing = d `GSOM.hasLabel` bmu
        report' = LearningReport
                    {
                      lLearningRate = GSOM.currentLearningRate d,
                      lNew = not existing,
                      lResponse = r,
                      lBmu = GSOM.cBmu report,
                      lNovelty = GSOM.cNovelty report,
                      lAdjNovelty = GSOM.cAdjNovelty report,
                      lDetails = GSOM.cDetails report
                    }

-- | Returns the set of scenarios for which this predictor has response
--   models.
scenarios :: Predictor t a -> [[GSOM.Label]]
scenarios = map (labels . snd) . M.toList . SOM.modelMap

-- | Returns @True@ if the predictor has a response for the scenario;
--   returns @False@ otherwise.
hasScenario :: Predictor t a -> [GSOM.Label] -> Bool
hasScenario p ls = ls `elem` scenarios p

actions :: Eq a => Predictor t a -> [a]
actions = nub . map (action . snd) . M.toList . SOM.modelMap

-- | Remove all response models from the predictor that refer to
--   classifier models that are not in the provided list.
filterLabels
  :: (SOM.Adjuster t, Eq a)
  => [GSOM.Label] -> Predictor t a -> Predictor t a
filterLabels ls = SOM.filter (\r -> null (labels r \\ ls))
