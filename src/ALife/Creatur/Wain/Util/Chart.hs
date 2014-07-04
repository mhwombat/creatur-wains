------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Util.Chart
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
-- module ALife.Creatur.Wain.Util.Chart where

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Data.List.Split
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens

chart :: (Ord a, PlotValue a) => String -> String -> [(a,a)] -> Renderable ()
chart _ h2 xs = toRenderable layout
  where
    plot1 = plot_lines_values .~ [xs]
              $ plot_lines_style  . line_color .~ opaque blue
              -- $ plot_lines_title .~ h2
              $ def

    layout = layout_title .~ "h2"
           $ layout_plots .~ [toPlot plot1]
           $ def

parseCSV :: String -> (String, String, [(Double, Double)])
parseCSV xss = (h1, h2, zs)
  where (hs,yss) = extractValues . tokenise $ xss
        (h1:h2:_) = hs
        zs = map (\(x:y:_) -> (x,y)) yss

tokenise :: String -> [[String]]
tokenise = map (splitOn ",") . lines

extractValues :: [[String]] -> ([String], [[Double]])
extractValues xss = (headings, values)
  where (headings:xs) = xss
        values = map (map read) xs

main :: IO ()
main = do
  rawData <- getContents
  let (h1, h2, values) = parseCSV rawData
  let myChart = chart h1 h2 values
  _ <- renderableToFile def myChart "example1_big.eps"
  return ()
