------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.AgentCSV
-- Copyright   :  (c) 2018-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Analyse a wain and generate a report.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module ALife.Creatur.Wain.AgentCSV
  (
    fetchObjects,
    agentToCSV
  ) where

import           ALife.Creatur.Wain
import qualified ALife.Creatur.Wain.Statistics   as S
import           Control.Monad                   (filterM)
import qualified Data.ByteString                 as BS
import qualified Data.Datamining.Clustering.SGM4 as SOM
import qualified Data.Serialize                  as DS
import           System.Directory                (listDirectory)
import           System.FilePath.Posix           (combine)
import           System.Posix                    (isDirectory, isRegularFile)
import           System.Posix.Files              (getFileStatus)

agentToCSV
  :: (SOM.Adjuster ct, S.Statistical ct, SOM.Adjuster pt, S.Statistical pt,
     Ord a, S.Statistical m)
    => Wain ct pt p a m -> IO ()
agentToCSV a = mapM_ (putStrLn . f) ss
  where agentName = name a
        ss = S.stats a
        f s = agentName ++ "," ++ S.name s ++ "," ++ show (S.value s)
