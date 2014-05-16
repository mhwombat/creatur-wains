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
    predict,
    learnLabel,
    observeAction,
    reflect,
    imprint,
    randomBrain,
    brainOK
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import qualified ALife.Creatur.Genetics.BRGCWord8 as G
import ALife.Creatur.Genetics.Diploid (Diploid)
import qualified ALife.Creatur.Wain.Classifier as C
import qualified ALife.Creatur.Wain.Decider as D
import ALife.Creatur.Wain.Condition (Condition, happiness)
import ALife.Creatur.Wain.Response (Response(..), setOutcome)
import ALife.Creatur.Wain.Scenario (Scenario(..))
import ALife.Creatur.Wain.Statistics (Statistical, stats, prefix)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Random (Rand, RandomGen)
import Data.Datamining.Pattern (Pattern, Metric)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)
import System.Random (Random)

data Brain p a = Brain
  {
    -- | Component that categorises and identifies patterns
    classifier :: C.Classifier p,
    -- | Component that makes decisions
    decider :: D.Decider a,
    -- | The last situation the agent was in, and the agent's response
    lastResponse :: Maybe (Response a)
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
  where classifierOK = C.somOK $ classifier b
        deciderOK = D.somOK $ decider b

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
  where (label, sig, c') = C.classify (classifier b) s
        b' = b { classifier = c' }

predict :: (Eq a) => Brain p a -> Response a -> Response a
predict b r = D.predict (decider b) r

learnLabel :: (Pattern p, Metric p ~ Double) => p -> C.Label -> Brain p a -> Brain p a
learnLabel p l b = b { classifier=C.learn p l (classifier b) }

observeAction :: (Pattern p, Metric p ~ Double, Eq a, Enum a, Bounded a)
    => Scenario -> a -> Brain p a -> Brain p a
observeAction s a b = b { lastResponse = Just r }
  where r = Response s a Nothing
  
-- lastResponse : Brain p a -> Maybe (Response a)
-- lastResponse = lastResponse

reflect
  :: (Pattern p, Metric p ~ Double, Eq a)
    => Condition -> Brain p a -> Brain p a
reflect cAfter (Brain c d lr) =
  case lr of
    Nothing -> Brain c d Nothing
    Just r  -> Brain c d' (Just r')
                where r' = r `setOutcome` deltaH
                      d' = D.feedback d r'
                      hBefore = happiness . condition $ scenario r
                      hAfter = happiness cAfter
                      deltaH = hAfter - hBefore

imprint
  :: (Pattern p, Metric p ~ Double, Eq a)
    => Brain p a -> Brain p a
imprint (Brain c d lr) =
  case lr of
    Nothing -> Brain c d Nothing
    Just r  -> Brain c d' (Just r')
                where r' = r `setOutcome` 1.0
                      d' = D.feedback d r'
