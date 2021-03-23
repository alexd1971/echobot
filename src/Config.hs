{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Aeson
import Data.Text
import Data.Yaml.Config

data Config = Config
  { repeatMessage :: Text,
    helpMessage :: Text,
    repeatNumber :: Int,
    logLevel :: String
  } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .: "repeat-message"
    <*> v .: "help-message"
    <*> v .: "repeat-number"
    <*> v .: "log-level"

configFile :: FilePath
configFile = "config.yaml"

readConfig :: IO Config
readConfig = loadYamlSettings [configFile] [] ignoreEnv
