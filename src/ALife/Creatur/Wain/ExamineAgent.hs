------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ExamineAgent
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Analyse a wain and generate a report.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.ExamineAgent
  (
    fetchObjects,
    examine
  ) where

import           ALife.Creatur.Wain
import           ALife.Creatur.Wain.Pretty
import           ALife.Creatur.Wain.Report
import           Control.Monad                           (filterM)
import qualified Data.ByteString                         as BS
import qualified Data.Datamining.Clustering.SGM4Internal as SOM
import qualified Data.Serialize                          as DS
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
     >>=
       filterM (fmap isRegularFile . getFileStatus) . map (combine f))
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
