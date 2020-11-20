{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module PlateTypes where

import GHC.Generics

import Data.Yaml.Aeson
import Data.Aeson.Types
import Control.Lens

data Transform = Transform
  { _inputDir :: FilePath
  , _outputDir :: FilePath
  } deriving (Show, Generic)

(makeLenses ''Transform)

instance FromJSON Transform where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}
instance ToJSON Transform where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 1}

data PlateTemplate = PlateTemplate
  { _template :: Maybe FilePath
  , _templates :: Maybe [FilePath]
  , _transform :: Maybe Transform
  , _vars  :: Object
  } deriving (Show, Generic)

(makeLenses ''PlateTemplate)

instance FromJSON PlateTemplate where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}
instance ToJSON PlateTemplate where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 1}

