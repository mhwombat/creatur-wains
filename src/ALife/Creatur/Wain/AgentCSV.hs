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
import           ALife.Creatur.Wain.Pattern    (Pattern)
import qualified ALife.Creatur.Wain.Statistics as S
import           Control.Monad                 (filterM, liftM)
import qualified Data.ByteString               as BS
import qualified Data.Serialize                as DS
import           System.Directory              (listDirectory)
import           System.FilePath.Posix         (combine)
import           System.Posix                  (isDirectory, isRegularFile)
import           System.Posix.Files            (getFileStatus)

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
    >>= filterM (liftM isRegularFile . getFileStatus) . map (combine f))
  >>= mapM fetchObject

fetchObject :: DS.Serialize b => FilePath -> IO b
fetchObject f = do
  x <- BS.readFile f
  let (Right w) = DS.decode x
  return w

agentToCSV
  :: (Pattern p, Ord a, S.Statistical m)
    => Wain p a m -> IO ()
agentToCSV a = mapM_ (putStrLn . f) ss
  where agentName = name a
        ss = S.stats a
        f s = agentName ++ "," ++ S.name s ++ "," ++ show (S.value s)
