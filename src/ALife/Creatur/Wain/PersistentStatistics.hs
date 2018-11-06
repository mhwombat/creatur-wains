------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.PersistentStatistics
-- Copyright   :  (c) Amy de Buitléir 2013-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Statistical calculations
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
module ALife.Creatur.Wain.PersistentStatistics
  (
    updateStats,
    readStats,
    clearStats
  ) where

import           ALife.Creatur.Universe
    (Universe, writeToLog)
import qualified ALife.Creatur.Wain.Statistics as Stats
import           Control.Monad.IO.Class
    (liftIO)
import           Control.Monad.State
    (StateT)
import qualified Data.Serialize                as DS
import           System.Directory
    (createDirectoryIfMissing, doesFileExist)
import           System.FilePath
    (dropFileName)
--import System.IO (hGetContents, withFile, Handle, IOMode(ReadMode))
--import Text.Read (readEither)
import qualified Data.ByteString               as BS

-- | Updates the stored statistics.
updateStats :: Universe u => [Stats.Statistic] -> FilePath -> StateT u IO ()
updateStats x f = do
  xs <- readStats f
  writeStats f (x:xs)

-- | Clears the stored statistics.
clearStats :: Universe u => FilePath -> StateT u IO ()
clearStats f = writeStats f []

-- | Returns the stored statistics.
readStats :: Universe u => FilePath -> StateT u IO [[Stats.Statistic]]
readStats f = do
  fExists <- liftIO $ doesFileExist f
  if fExists
    then do
      b <- liftIO $ BS.readFile f
      let x = DS.decode b
      -- x <- liftIO $ withFile f ReadMode readStats' -- closes file ASAP
      case x of
        Left msg  -> do
          writeToLog $ "Unable to read stats from " ++ f ++ ": " ++ msg
          return []
        Right xs -> return xs
    else return []

-- readStats' :: Handle -> IO (Either String [[Statistic]])
-- readStats' h = do
--   s <- hGetContents h
--   let x = readEither s
--   case x of
--     Left msg -> return $ Left (msg ++ "\"" ++ s ++ "\"")
--     Right c  -> return $ Right c


writeStats :: Universe u => FilePath -> [[Stats.Statistic]] -> StateT u IO ()
writeStats f xs = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  liftIO $ BS.writeFile f (DS.encode xs)

-- -- | Summarises the stored statistics and writes them to the log.
-- summarise :: Universe u => [[Stats.Statistic]] -> StateT u IO ()
-- summarise xss = mapM_ f $ summarise' xss
--   where f s = writeToLog $ "Summary - " ++ s
--         summarise' xs = intercalate "," $ map pretty xs
