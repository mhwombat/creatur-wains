------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.LearningParamsInternal
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private LearningParams internals.
-- Most developers should use LearningParams instead.
-- This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ALife.Creatur.Wain.LearningParamsInternal where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import           ALife.Creatur.Gene.Numeric.Util         (inRange, intersection)
import qualified ALife.Creatur.Genetics.BRGCWord8        as G
import           ALife.Creatur.Genetics.Diploid          (Diploid)
import           ALife.Creatur.Wain.Pretty               (pretty)
import           ALife.Creatur.Wain.Report               (Report, report)
import           ALife.Creatur.Wain.Statistics           (Statistical, dStat,
                                                          iStat, stats)
import           Control.DeepSeq                         (NFData)
import           Control.Monad.Random                    (Rand, RandomGen,
                                                          getRandomR, runRand)
import           Data.Serialize                          (Serialize)
import           Data.Word                               (Word32)
import           GHC.Generics                            (Generic)
import           Test.QuickCheck                         (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen                     (Gen (MkGen))

--
--
-- Learning parameters used by the genetic adjuster
--
--

-- | Private constructor.
--   @'LearningParams' r0 rf tf@ defines the shape of the learning
--   function.
--   When t = 0, the learning rate is r0.
--   Over time the learning rate decays,
--   so that when t = tf, the learning rate is rf.
--   Normally the parameters are chosen such that:
--     0 < r0 <= 1
--     0 < rf <= r0
data LearningParams = LearningParams UI.Double UI.Double Word32
  deriving (Eq, Show, Read, Generic, Serialize, Diploid, NFData)

instance G.Genetic LearningParams where
  put (LearningParams r0 rf tf)
    = G.put r0 >> G.put rf >> G.put tf
  get = do
    r0 <- G.get
    rf <- G.get
    tf <- G.get
    -- Use the safe constructor!
    case mkLearningParams <$> r0 <*> rf <*> tf of
      Left msgs -> return $ Left msgs
      Right p   -> return p

instance Statistical LearningParams where
  stats (LearningParams r0 rf tf)
    = [ dStat "r0" (UI.wide r0), dStat "rf" (UI.wide rf), iStat "tf" tf]

instance Report LearningParams where
  report (LearningParams r0 rf tf)
    = ["r0: " ++ pretty r0, "rf: " ++ pretty rf, "tf: " ++ pretty tf]

-- | @'mkLearningParams' r0 rf tf@ defines the shape of the learning
--   function.
--   When t = 0, the learning rate is r0.
--   Over time the learning rate decays exponentially,
--   so that when t = tf, the learning rate is rf.
--   Normally the parameters are chosen such that:
--     0 < r0 <= 1
--     0 < rf <= r0
mkLearningParams
  :: UI.Double -> UI.Double -> Word32 -> Either [String] LearningParams
mkLearningParams r0 rf tf
  | r0 == 0    = Left ["r0 must be > 0"]
  | rf > r0   = Left ["rf must be <= r0"]
  | otherwise = Right $ LearningParams r0 rf tf

-- | @'toLearningFunction' p t@ returns the learning rate at time @t@,
--   given an exponential learning function with parameters @p@.
toLearningFunction :: LearningParams -> Word32 -> UI.Double
toLearningFunction (LearningParams r0 rf tf) t
  | inRange (0,1) r = UI.narrow r
  | otherwise       = error $ "toLearningFunction: out of bounds"
                                ++ " r0=" ++ show r0
                                ++ " rf=" ++ show rf
                                ++ " tf=" ++ show tf
                                ++ " t=" ++ show t
                                ++ " r=" ++ show r
  where r = r0' * exp (-d*t')
        r0' = UI.wide r0
        rf' = UI.wide rf
        t' = fromIntegral t
        tf' = fromIntegral tf
        d = log (r0'/rf') / tf'

--
--
-- Ranges for learning parameters
--
--

-- | A set of parameters to constrain the result when generating
--   random learning functions.
data LearningParamRanges = LearningParamRanges
  {
    -- | The range from which the initial learning rate (at t=0)
    --   should be chosen.
    r0Range :: (UI.Double, UI.Double),
    -- | The range from which the final learning rate (at t=@tf@)
    --   should be chosen.
    rfRange :: (UI.Double, UI.Double),
    -- | The range from which the final time should be chosen.
    tfRange :: (Word32, Word32)
  } deriving (Show, Read, Eq)

-- | Range of values permitted for @r0@
r0RangeLimits :: (UI.Double, UI.Double)
r0RangeLimits = (UI.narrow (1/65535), 1)

-- | Range of values permitted for @rf@
rfRangeLimits :: (UI.Double, UI.Double)
rfRangeLimits = (UI.narrow (1/65535), 1)

-- | Range of values permitted for @rf@
tfRangeLimits :: (Word32, Word32)
tfRangeLimits = (1, maxBound)

-- | Returns a set of parameters which will permit the broadest possible
--   set of random decaying gaussian functions for a SOM.
widestLearningParamRanges :: LearningParamRanges
widestLearningParamRanges =
  LearningParamRanges r0RangeLimits rfRangeLimits tfRangeLimits

-- | @'randomLearningParams'
--   ('LearningParamRanges' r0Range rfRange tRange)@
--   returns a random decaying function that can be used as the
--   learning function for an SGM.
--   The parameters of the gaussian will be chosen such that:
--
--   * r0 is in r0Range, but also 0 < r0 <= 1
--   * rf is in rfRange, but also 0 < rf <= 1
--   * tf is in tfRange, but also 0 < tf
randomLearningParams
  :: RandomGen g
    => LearningParamRanges
      -> Rand g LearningParams
randomLearningParams p = do
  r0 <- getRandomR . intersection r0RangeLimits . r0Range $ p
  rf <- getRandomR . intersection (0, r0) . intersection rfRangeLimits . rfRange $ p
  tf <- getRandomR . intersection tfRangeLimits . tfRange $ p
  let Right x = mkLearningParams r0 rf tf
  return x

instance Arbitrary LearningParams where
  arbitrary = do
    p <- arbitrary
    MkGen (\r _ -> let (x,_) = runRand (randomLearningParams p) r in x)

instance Arbitrary LearningParamRanges where
  arbitrary = do
    r0start <- arbitrary
    r0stop <- arbitrary
    rfstart <- arbitrary
    rfstop <- arbitrary
    tfstart <- arbitrary
    tfstop <- arbitrary
    return $ LearningParamRanges (r0start,r0stop) (rfstart,rfstop)
               (tfstart,tfstop)
