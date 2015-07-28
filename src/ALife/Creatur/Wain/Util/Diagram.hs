------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Util.Diagram
-- Copyright   :  (c) Amy de Buitléir 2014-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A collection of tools for making SVG diagrams about wains.
--
------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module ALife.Creatur.Wain.Util.Diagram
  (
    assessmentDiagram,
    visualiseAssessment,
    predictorDiagram,
    visualisePredictorModel
  )where

import Data.Colour.SRGB
import Data.List.Split
import Data.Word
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text
import Numeric

redGreenRect :: Word8 -> Colour Double
redGreenRect x = sRGB x' (1-x') 0
  where x' = fromIntegral x / 255

blueBlackRect :: Word8 -> Colour Double
blueBlackRect x = sRGB 0 0 x'
  where x' = fromIntegral x / 255

string2hexs :: String -> [Word8]
string2hexs = map readOneHex . chunksOf 2

readOneHex :: String -> Word8
readOneHex s = n
  where (n,_) = head $ readHex s

colour2rect
  :: (HasStyle b, Transformable b, TrailLike b, V b ~ R2)
    => Colour Double -> b
colour2rect c = rect 1 2 # fc c # lw none

objectBar
  :: (HasOrigin c, Juxtaposable c, HasStyle c, Transformable c,
    TrailLike c, Semigroup c, Monoid c, V c ~ R2)
      => String -> c
objectBar = hcat . map (colour2rect . redGreenRect) . string2hexs

statusBar
  :: (HasOrigin c, Juxtaposable c, HasStyle c, Transformable c,
    TrailLike c, Semigroup c, Monoid c, V c ~ R2)
      => String -> c
statusBar = hcat . map (colour2rect . blueBlackRect) . string2hexs

spacer
  :: (HasOrigin c, Juxtaposable c, HasStyle c, Transformable c,
    TrailLike c, Semigroup c, Monoid c, V c ~ R2)
      => c
spacer = colour2rect $ sRGB 1 1 1

parseAssessment :: String -> [String]
parseAssessment = splitOn "|"

parsePredictorModel :: String -> [String]
parsePredictorModel s = index:ts
  where (index:t:_) = splitOn "=" s
        ts = splitOn "|" t

label
  :: (Renderable Diagrams.TwoD.Text.Text b, Renderable (Path R2) b)
     => Int -> String -> Diagram b R2
label n s = text s <> rect (fromIntegral n) 2 # lw none

visualiseAssessment :: [String] -> Diagram B R2
visualiseAssessment (dObj:iObj:state:_) =
  objectBar dObj ||| spacer||| objectBar iObj ||| spacer
    ||| statusBar state
visualiseAssessment _ = text "invalid assessment format"

assessmentDiagram :: String -> Diagram B R2
assessmentDiagram =
  vcat . map (visualiseAssessment . parseAssessment) . lines

visualisePredictorModel :: [String] -> Diagram B R2
visualisePredictorModel (index:dObj:iObj:state:action:outcome:_) =
  label 4 index ||| objectBar dObj ||| spacer||| objectBar iObj
    ||| spacer ||| statusBar state ||| label 10 action
    ||| label 6 outcome
visualisePredictorModel _ = text "invalid predictor model format"

predictorDiagram :: String -> Diagram B R2
predictorDiagram =
  vcat . map (visualisePredictorModel . parsePredictorModel) . lines
