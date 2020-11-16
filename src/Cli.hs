{-# LANGUAGE OverloadedStrings #-}
module Cli (runTemplate) where

import System.IO
import System.IO.Error
import System.FilePath
import System.Directory
import System.Exit

import Text.Ginger
import Text.Ginger.Parse
import Text.Ginger.Run
import Data.Text (Text, intercalate)
import qualified Data.Text.IO as TIO
import Data.Yaml
import Control.Lens
import Control.Monad.Writer

import PlateTypes

loadFileMay :: FilePath -> IO (Maybe String)
loadFileMay fn =
  tryIOError (openFile fn ReadMode >>= hGetContents) >>= \e ->
    case e of
      Right contents -> pure (Just contents)
      Left err -> do
        print err
        return Nothing

runTemplate :: FilePath -> IO ()
runTemplate fileName = do
  case takeExtensions fileName of
    ".plate.yml" -> do
      result <- handlePlateTemplate fileName
      TIO.putStr $ intercalate "---\n" result
    _            -> undefined

parseTemplateWithContext :: FilePath -> Object -> IO Text
parseTemplateWithContext fileName context = do
  parsed <- parseGingerFile loadFileMay fileName
  case parsed of
   Left err -> die $ show err
   Right a -> pure $ easyRender context a

handlePlateTemplate :: FilePath -> IO [Text]
handlePlateTemplate fileName = do
  plateFile <- decodeFileThrow fileName 
  case (plateFile ^. template, plateFile ^. templates) of
    (Just _, Just _) -> putStr "ERROR" *> pure []
    (Just single, _) -> pure <$> doThing (plateFile ^. vars) fileName single
    (_, Just multiple) -> sequence $ (doThing (plateFile ^. vars) fileName) <$> multiple
  where
    doThing :: Object -> FilePath -> FilePath -> IO Text
    doThing vars plateFileLocation templateLocation = do
      normalizedName <- normalizeToDir plateFileLocation
      let name = joinPath $ [normalizedName, templateLocation]
      parseTemplateWithContext name vars

    normalizeToDir :: FilePath -> IO FilePath
    normalizeToDir path = do
      isFile <- doesFileExist path
      pure $ if isFile then
             dropFileName path
           else
             path
