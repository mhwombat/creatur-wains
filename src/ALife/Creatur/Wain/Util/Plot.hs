------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Util.Plot
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
-- module ALife.Creatur.Wain.Util.Plot where

-- import Graphics.Rendering.Chart
-- import Data.Colour
-- import Data.Colour.Names
-- import Data.Default.Class
-- import Data.List.Split
-- import Graphics.Rendering.Chart.Backend.Cairo
-- import Control.Lens

-- chart :: (Ord a, PlotValue a) => String -> String -> [(a,a)] -> Renderable ()
-- chart _ h2 xs = toRenderable layout
--   where
--     plot1 = plot_lines_values .~ [xs]
--               $ plot_lines_style  . line_color .~ opaque blue
--               -- $ plot_lines_title .~ h2
--               $ def

--     layout = layout_title .~ "h2"
--            $ layout_plots .~ [toPlot plot1]
--            $ def

--Create some data:

ln = 25
ts = linspace ln (0,1)
rs = randomVector 0 Exponential ln
 
ss = sin (15*2*pi*ts)
ds = 0.25*rs + ss
es = constant (0.25*(stddev rs)) ln
 
fs :: Double -> Double
fs = sin . (15*2*pi*)

test_graph2 = do
         plot (ts,[point (ds,es) (Cross,red),line fs blue])
         title "Testing plot package:"
         subtitle "with 1 second of a 15Hz sine wave"
         xlabel "time (s)"
         ylabel "amplitude"
         yrange Linear (-1.25) 1.25

test_render :: (Double,Double) -> Render ()
test_render = render test_graph

display :: ((Int,Int) -> C.Render ()) -> IO ()
display r = do
   initGUI       -- is start

   window <- windowNew
   set window [ windowTitle := "Cairo test window"
              , windowDefaultWidth := 600
              , windowDefaultHeight := 400
              , containerBorderWidth := 1
              ]

--   canvas <- pixbufNew ColorspaceRgb True 8 300 200
--   containerAdd window canvas
   frame <- frameNew
   containerAdd window frame
   canvas <- drawingAreaNew
   containerAdd frame canvas
   widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

   widgetShowAll window 

   on canvas exposeEvent $ tryEvent $ do s <- liftIO $ widgetGetSize canvas
                                         drw <- liftIO $ widgetGetDrawWindow canvas
                                         --dat <- liftIO $ takeMVar d
                                         --liftIO $ renderWithDrawable drw (circle 50 10)
                                         liftIO $ renderWithDrawable drw (r s)

   onDestroy window mainQuit
   mainGUI

          
main = display $ render figure

test = writeFigure PDF "test.pdf" (400,400) figure

-- parseCSV :: String -> (String, String, [(Double, Double)])
-- parseCSV xss = (h1, h2, zs)
--   where (hs,yss) = extractValues . tokenise $ xss
--         (h1:h2:_) = hs
--         zs = map (\(x:y:_) -> (x,y)) yss

-- tokenise :: String -> [[String]]
-- tokenise = map (splitOn ",") . lines

-- extractValues :: [[String]] -> ([String], [[Double]])
-- extractValues xss = (headings, values)
--   where (headings:xs) = xss
--         values = map (map read) xs

-- main :: IO ()
-- main = do
--   rawData <- getContents
--   let (h1, h2, values) = parseCSV rawData
--   let myChart = chart h1 h2 values
--   _ <- renderableToFile def myChart "example1_big.eps"
--   return ()
