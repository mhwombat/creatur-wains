------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ExamineAgent
-- Copyright   :  (c) Amy de Buitléir 2013-2017
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

import ALife.Creatur (agentId)
import ALife.Creatur.Wain
import ALife.Creatur.Wain.Brain
import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.Pretty
import ALife.Creatur.Wain.UnitInterval
import qualified Data.ByteString as BS
import qualified Data.Serialize as DS
import Control.Lens
import Control.Monad (liftM, filterM)
import System.Directory (listDirectory)
import System.FilePath.Posix (combine)
import System.Posix (isRegularFile, isDirectory)
import System.Posix.Files (getFileStatus)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

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
  map (combine f) <$> listDirectory f
    >>= filterM (liftM isRegularFile . getFileStatus)
    >>= mapM fetchObject

fetchObject :: DS.Serialize b => FilePath -> IO b
fetchObject f = do
  x <- BS.readFile f
  let (Right w) = DS.decode x
  return w

examine
  :: (Pretty p, Pretty a, Pretty ct, Pretty pt)
    => Wain p ct pt m a -> IO ()
examine a = do
  putStrLn $ "name: " ++ view name a
  -- appearance
  -- brain
  putStrLn $ "devotion: " ++ printf "%5.3f" (uiToDouble $ view devotion a)
  putStrLn $ "ageOfMaturity: " ++ pretty (view ageOfMaturity a)
  putStrLn $ "passionDelta: " ++ pretty (view passionDelta a)
  putStrLn $ "boredomDelta: " ++ pretty (view boredomDelta a)
  putStrLn $ "energy: " ++ printf "%5.3f" (uiToDouble $ view energy a)
  putStrLn $ "passion: " ++ printf "%5.3f" (uiToDouble $ view passion a)
  putStrLn $ "age: " ++ pretty (view age a)
  -- litter
  putStrLn $ "total # children borne: "
    ++ pretty (view childrenBorneLifetime a)
  putStrLn $ "total # children weaned: "
    ++ pretty (view childrenWeanedLifetime a)
  putStrLn $ "litter size: " ++ pretty (length . view litter $ a)
  putStrLn $ "classifier SQ: " ++ pretty (schemaQuality . view classifier . view brain $ a)
  putStrLn $ "predictor SQ: " ++ pretty (schemaQuality . view predictor . view brain $ a)
  putStrLn $ "DSQ: " ++ pretty (decisionQuality . view brain $ a)
  putStrLn $ "Max. number of classifier models: " ++ pretty (maxSize . view classifier . view brain $ a)
  putStrLn $ "Number of classifier models: " ++ pretty (numModels . view classifier . view brain $ a)
  putStrLn $ "Classifier learning function " ++ pretty (view learningParams . view classifier . view brain $ a)
  putStrLn $ "Classifier tweaker: " ++ pretty (view tweaker . view classifier . view brain $ a)
  putStrLn $ "Classifier counts: " ++ pretty (counterMap . view classifier . view brain $ a)
  mapM_ putStrLn $ describeClassifierModels a
  putStrLn $ "Max. number of predictor models: " ++ pretty (maxSize . view predictor . view brain $ a)
  putStrLn $ "Number of predictor models: " ++ pretty (numModels . view predictor . view brain $ a)
  putStrLn $ "Predictor learning function " ++ pretty (view learningParams . view predictor . view brain $ a)
  putStrLn $ "Predictor tweaker: " ++ pretty (view tweaker . view predictor . view brain $ a)
  putStrLn $ "Predictor counts: " ++ pretty (counterMap . view predictor . view brain $ a)
  mapM_ putStrLn $ describePredictorModels a
  -- putStrLn "--------"
  -- putStrLn "Raw data"
  -- putStrLn "--------"
  -- putStrLn $ pretty a

describeClassifierModels :: Pretty p => Wain p ct pt m a -> [String]
describeClassifierModels w = map f ms
  where ms = M.toList . modelMap . view (brain . classifier) $ w
        f (l, r) = agentId w ++ "'s classifier model "
                     ++ pretty l ++ " " ++ pretty r

describePredictorModels
  :: (Pretty p, Pretty a)
    => Wain p ct pt m a -> [String]
describePredictorModels w = map f ms
  where ms = M.toList . modelMap . view (brain . predictor) $ w
        f (l, r) = agentId w ++ "'s predictor model "
                     ++ pretty l ++ ": " ++ pretty r

-- formatVector :: String -> [Double] -> String
-- formatVector fmt = unwords . map (printf fmt)
