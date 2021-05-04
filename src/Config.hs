{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Aeson
  ( FromJSON (parseJSON),
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (emptyObject)
import Data.Text (Text)
import Data.Yaml.Config (ignoreEnv, loadYamlSettings)

data RepeatConf = RepeatConf {message :: Text, defaultCount :: Int} deriving (Eq, Show)

instance FromJSON RepeatConf where
  parseJSON = withObject "RepeatConf" $ \v ->
    RepeatConf
      <$> v .:? "message" .!= "Set repeat count"
      <*> v .:? "default_count" .!= 1

data TelegramConf = TelegramConf {apiUrl :: Text, apiKey :: Text} deriving (Eq, Show)

instance FromJSON TelegramConf where
  parseJSON = withObject "TelegramConf" $ \v ->
    TelegramConf
      <$> v .:? "api_url" .!= "https://api.telegram.org"
      <*> v .:? "api_key" .!= mempty

data Config = Config
  { helpMessage :: Text,
    repeat :: RepeatConf,
    logLevel :: String,
    telegram :: TelegramConf
  }
  deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj ->
    Config
      <$> obj .:? "help_message" .!= "Echo Bot"
      <*> do
        maybeValue <- obj .:? "repeat"
        case maybeValue of
          Nothing -> parseJSON emptyObject
          Just value -> return value
      <*> obj .:? "log_level" .!= "info"
      <*> do
        maybeValue <- obj .:? "telegram"
        case maybeValue of
          Nothing -> parseJSON emptyObject
          Just value -> return value

configFile :: FilePath
configFile = "config.yaml"

readConfig :: IO Config
readConfig = loadYamlSettings [configFile] [] ignoreEnv
