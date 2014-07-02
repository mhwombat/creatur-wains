------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Util.DeciderDiagram
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleContexts #-}

import ALife.Creatur.Wain.Util.Diagram
import Diagrams.Backend.SVG.CmdLine

main :: IO ()
main = getContents >>= mainWith . deciderDiagram

