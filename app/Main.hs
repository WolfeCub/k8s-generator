{-# LANGUAGE TypeApplications, DataKinds #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Cli

data Commands = Commands
  { fileName      :: String
  }

commands :: Parser Commands
commands = Commands
      <$> argument str
          ( metavar "FILENAME"
         <> help "File for processing" )

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (commands <**> helper)
      ( fullDesc
     <> progDesc "Generic temPLATEs for variable & secret injection"
     <> header "plate - Generic temPLATEs for variable & secret injection" )

greet :: Commands -> IO ()
greet (Commands fileName) = runTemplate fileName
greet _ = return ()
