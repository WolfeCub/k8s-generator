{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module PlateTypes where

import GHC.Generics

import Data.Yaml.Aeson
import Data.Aeson.Types
import Control.Lens


data PlateTemplate = PlateTemplate
  { _template :: Maybe FilePath
  , _templates :: Maybe [FilePath]
  , _vars  :: Object
  } deriving (Show, Generic)

(makeLenses ''PlateTemplate)

instance FromJSON PlateTemplate where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}
instance ToJSON PlateTemplate where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 1}

