------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Util.PredictorDiagram
-- Copyright   :  (c) Amy de Buitléir 2014-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import ALife.Creatur.Wain.Util.Diagram
import Diagrams.Backend.SVG.CmdLine

main :: IO ()
main = getContents >>= mainWith . predictorDiagram
