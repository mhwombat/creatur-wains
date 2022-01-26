------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Util
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Read wains from files.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Util
  (
    fetchObjects,
    examine,
    toCSV,
    replay
  ) where

import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import           ALife.Creatur.Genetics.BRGCWord8        (Genetic)
import           ALife.Creatur.Genetics.Diploid          (Diploid)
import           ALife.Creatur.Wain
import           ALife.Creatur.Wain.Muser                (Action, Muser)
import           ALife.Creatur.Wain.Pretty               (Pretty)
import           ALife.Creatur.Wain.Report               (Report, report)
import qualified ALife.Creatur.Wain.Response             as R
import qualified ALife.Creatur.Wain.Statistics           as S
import           Control.Monad                           (filterM)
import qualified Data.ByteString                         as BS
import qualified Data.Datamining.Clustering.SGM4         as SOM
import qualified Data.Serialize                          as DS
import           Data.Word                               (Word32)
import           System.Directory                        (listDirectory)
import           System.FilePath.Posix                   (combine)
import           System.Posix                            (isDirectory,
                                                          isRegularFile)
import           System.Posix.Files                      (getFileStatus)

fetchObjects :: DS.Serialize b => FilePath -> IO [b]
fetchObjects f = do
  dir <- isDirectory <$> getFileStatus f
  if dir
    then fetchAllObjects f
    else do
      w <- fetchObject f
      return [w]

fetchAllObjects :: DS.Serialize b => FilePath -> IO [b]
fetchAllObjects f =
  (listDirectory f
    >>= filterM (fmap isRegularFile . getFileStatus) . map (combine f))
  >>= mapM fetchObject

fetchObject :: DS.Serialize b => FilePath -> IO b
fetchObject f = do
  x <- BS.readFile f
  case DS.decode x of
    Right w -> return w
    Left s  -> error $ "Cannot read " ++ f ++ ". " ++ s

examine
  :: (SOM.Adjuster ct, Report ct, SOM.Adjuster pt, Report pt,
     Pretty p, Eq a, Pretty a, Pretty m)
    => Wain ct pt p a m -> IO ()
examine = mapM_ putStrLn . report

toCSV
  :: (SOM.Adjuster ct, S.Statistical ct, SOM.Adjuster pt, S.Statistical pt,
     Ord a, S.Statistical m)
    => Wain ct pt p a m -> IO ()
toCSV a = mapM_ (putStrLn . f) ss
  where agentName = name a
        ss = S.stats a
        f s = agentName ++ "," ++ S.name s ++ "," ++ show (S.value s)

replay
  :: (SOM.Adjuster ct, Diploid ct, Genetic ct, Show ct,
     SOM.PatternType ct ~ p, SOM.MetricType ct ~ UI.Double,
     SOM.TimeType ct ~ Word32,
     SOM.Adjuster pt, Diploid pt, Genetic pt, Show pt,
     SOM.PatternType pt ~ R.Response a, SOM.MetricType pt ~ UI.Double,
     SOM.TimeType pt ~ Word32,
     Pretty p, Show p, Diploid p, Genetic p,
     Diploid a, Eq a, Genetic a, Ord a, Pretty a, Show a,
     Muser m, Diploid m, Genetic m, Pretty m, Show m, Action m ~ a)
    => Wain ct pt p a m -> IO ()
replay = mapM_ putStrLn . snd . runLife
