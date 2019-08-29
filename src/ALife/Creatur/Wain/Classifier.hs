------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Classifier
-- Copyright   :  (c) Amy de Buitléir 2012-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A classifier based on a Kohonen Self-organising Map (SOM).
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ALife.Creatur.Wain.Classifier
  (
    S.Label,
    S.Difference,
    Classifier,
    ClassifierReport(..),
    buildClassifier,
    classifySetAndTrain,
    imprintSet,
    labels,
    bmus,
    diffs,
    prettyClassifierReport
  ) where

import           ALife.Creatur.Wain.ClassifierInternal
import qualified ALife.Creatur.Wain.GeneticSOM         as S
