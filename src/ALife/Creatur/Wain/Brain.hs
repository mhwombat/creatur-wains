------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Brain
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleContexts,
    StandaloneDeriving #-}
module ALife.Creatur.Wain.Brain
  (
    Brain(..),
    buildBrain,
    assessSituation,
    classify,
    recommendAction,
    learnLabel,
    learnAction,
    feedback,
    numberOfClassifierModels,
    numberOfDeciderModels,
    conflation,
    discrimination,
    classifierMap,
    deciderMap,
    randomBrain,
    brainOK,
    counterList
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Genetics.Diploid (Diploid)
import qualified ALife.Creatur.Wain.Classifier as C
import qualified ALife.Creatur.Wain.Decider as D
import ALife.Creatur.Wain.Condition (Condition)
import ALife.Creatur.Wain.Response (Response(..), setOutcome)
import ALife.Creatur.Wain.Scenario (Scenario(..))
import ALife.Creatur.Wain.Statistics (Statistical, stats, prefix)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Random (Rand, RandomGen)
import Data.Datamining.Pattern (Pattern, Metric)
import Data.Serialize (Serialize)
import Data.Word (Word8, Word16)
import GHC.Generics (Generic)
import Math.Geometry.Grid.Hexagonal (HexHexGrid)
import Math.Geometry.GridMap.Lazy (LGridMap)
import System.Random (Random)

data Brain p a = Brain
  {
    -- | Component that categorises and identifies patterns
    bClassifier :: C.Classifier p,
    -- | Component that makes decisions
    bDecider :: D.Decider a,
    -- | The last situation the agent was in, and the agent's response
    bLastResponse :: Maybe (Response a)
  } deriving (Generic)

deriving instance (Eq p, Eq a, Pattern p, Metric p ~ Double) 
    => Eq (Brain p a)

instance (Serialize p, Serialize a, Pattern p, Eq a, Metric p ~ Double)
    => Serialize (Brain p a)
         
instance (Diploid p, Diploid a, Pattern p, Eq a, Metric p ~ Double)
    => Diploid (Brain p a)

instance (Pattern p, Metric p ~ Double)
      => Statistical (Brain p a) where
  stats (Brain c d _) = map (prefix "classifier ") (stats c)
    ++ map (prefix "decider ") (stats d)

instance (Pattern p, Show p, Show (Metric p), Ord (Metric p), Show a, Eq a)
      => Show (Brain p a) where
  show (Brain c d lr) = "Brain (" ++ show c ++ ") (" ++ show d ++ ") ("
    ++ show lr ++ ")"

brainOK
  :: (Pattern p, Ord (Metric p), Metric p ~ Double, Eq a)
    => Brain p a -> Bool
brainOK b = classifierOK && deciderOK
  where classifierOK = C.somOK $ bClassifier b
        deciderOK = D.somOK $ bDecider b

-- | Construct a brain
buildBrain :: C.Classifier p -> D.Decider a -> Brain p a
buildBrain c d = Brain c d Nothing

instance (Genetic p, Genetic a, Pattern p, Metric p ~ Double, Eq a)
    => Genetic (Brain p a) where
  put (Brain c d _) = G.put c >> G.put d
  get = do
    c0 <- G.get
    let c = case c0 of
          (Left xs) -> Left ("Classifier:":xs)
          (Right c1) ->
            if C.numModels c1 == 0
               then Left ["Classifier has no models"]
               else Right c1
    d0 <- G.get
    let d = case d0 of
          (Left xs) -> Left ("Decider:":xs)
          (Right d1) ->
            if D.numModels d1 == 0
               then Left ["Decider has no models"]
               else Right d1
    return $ buildBrain <$> c <*> d

randomBrain
  :: (RandomGen g, Pattern p, Metric p ~ Double, Eq a, Random a)
    => Word8 -> [p] -> Word8 -> Rand g (Brain p a)
randomBrain classifierSize ps deciderSize = do
  c <- C.randomClassifier classifierSize ps
  let numClassifierModels = fromIntegral $ C.numModels c
  d <- D.randomDecider numClassifierModels deciderSize
  return $ buildBrain c d

assessSituation
  :: (Pattern p, Metric p ~ Double)
    => p -> p -> Condition -> Brain p a -> (C.Label, C.Label, Scenario, Brain p a)
assessSituation p1 p2 cond b = (l1, l2, sc, b2)
  where (l1, sig1, b1) = classify' p1 b
        (l2, sig2, b2) = classify' p2 b1
        sc = Scenario sig1 sig2 cond

-- | Find out how similar the input is to the models in the classifier.
classify
  :: (Pattern p, Metric p ~ Double)
    => p -> Brain p a -> (C.Label, Brain p a)
classify p b = (label, b')
  where (label, _, b') = classify' p b

classify'
  :: (Pattern p, Metric p ~ Double)
    => p -> Brain p a -> (C.Label, [Metric p], Brain p a)
classify' s b = (label, sig, b')
  where (label, sig, c') = C.classify (bClassifier b) s
        b' = b { bClassifier = c' }

recommendAction
  :: (Pattern p, Metric p ~ Double, Eq a, Enum a, Bounded a)
    => Scenario -> Brain p a -> (a, Brain p a)
recommendAction s (Brain c d _) = (action r, Brain c d (Just r))
  where r = D.recommendResponse d s

learnLabel :: (Pattern p, Metric p ~ Double) => p -> C.Label -> Brain p a -> Brain p a
learnLabel p l b = b { bClassifier=C.learn p l (bClassifier b) }

learnAction :: (Pattern p, Metric p ~ Double, Eq a, Enum a, Bounded a)
    => Scenario -> a -> Brain p a -> Brain p a
learnAction s a b = b { bLastResponse = Just r }
  where r = Response s a Nothing
  
-- lastResponse : Brain p a -> Maybe (Response a)
-- lastResponse = bLastResponse

feedback
  :: (Pattern p, Metric p ~ Double, Eq a)
    => Double -> Brain p a -> Brain p a
feedback deltaHappiness (Brain c d lr) =
  case lr of
    Nothing -> Brain c d Nothing
    Just r  -> Brain c d' (Just r')
                where r' = r `setOutcome` deltaHappiness
                      d' = D.feedback d r'

-- teach :: Brain p a -> Response a -> Brain p a
-- teach b r = b { bDecider = D.feedback (bDecider b) r } 

numberOfClassifierModels :: (Pattern p, Metric p ~ Double) => Brain p a -> Int
numberOfClassifierModels = C.numModels . bClassifier

numberOfDeciderModels :: Eq a => Brain p a -> Int
numberOfDeciderModels = D.numModels . bDecider

conflation :: Metric p ~ Double => Brain p a -> Double
conflation = C.conflation . bClassifier

discrimination
  :: (Pattern p, Metric p ~ Double)
     => Brain p a -> Int -> Double
discrimination b maxCategories
  = C.discrimination (bClassifier b) maxCategories

counterList
  :: (Pattern p, Metric p ~ Double)
    => Brain p a -> ([(C.Label,Word16)], [(D.Label,Word16)])
counterList b
  = (C.counterList $ bClassifier b, D.counterList $ bDecider b)

classifierMap
  :: (Pattern p, Metric p ~ Double)
    => Brain p a -> LGridMap HexHexGrid p
classifierMap = C.mindMap . bClassifier

deciderMap
  :: Eq a
    => Brain p a -> LGridMap HexHexGrid (Response a)
deciderMap = D.mindMap . bDecider
