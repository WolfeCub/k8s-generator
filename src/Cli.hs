{-# LANGUAGE OverloadedStrings #-}
module Cli (runTemplate) where

import Data.Maybe
import System.IO
import System.IO.Error
import System.FilePath
import System.Directory
import System.Exit

import Control.Monad.Reader
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

handlePlateTemplate :: FilePath -> IO [Text]
handlePlateTemplate fileName = do
  tmp <- decodeFileThrow fileName

  let templatesToProcess =
        case (tmp ^. template, tmp ^. templates) of
          (Just single, _) -> [single]
          (_, Just multi)  -> multi

  runReaderT (sequence $ normalizeAndParse fileName <$> templatesToProcess) tmp

  where
    normalizeAndParse :: FilePath -> FilePath -> ReaderT PlateTemplate IO Text
    normalizeAndParse plateFileLocation templateLocation = do
      normalizedName <- lift $ normalizeToDir plateFileLocation
      let name = joinPath $ [normalizedName, templateLocation]
      v <- view vars
      lift $ parseTemplateWithContext name v

    parseTemplateWithContext :: FilePath -> Object -> IO Text
    parseTemplateWithContext fileName context = do
      parsed <- parseGingerFile loadFileMay fileName
      case parsed of
       Left err -> die $ show err
       Right a -> pure $ easyRender context a

    normalizeToDir :: FilePath -> IO FilePath
    normalizeToDir path = do
      isFile <- doesFileExist path
      pure $ if isFile then
             dropFileName path
           else
             path
