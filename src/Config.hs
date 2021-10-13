{-# LANGUAGE OverloadedStrings #-}

module Config
  ( RepeatConf(..)
  , TelegramConf(..)
  , Config(..)
  , readConfig
  ) where

import Control.Exception (Exception(displayException))
import Data.Aeson
  ( FromJSON(parseJSON)
  , Result(Success)
  , (.!=)
  , (.:)
  , (.:?)
  , withObject
  )
import Data.Aeson.Types (emptyObject, parse, parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Yaml (decodeFileEither)
import System.Log.Logger (rootLoggerName, warningM)

data Config =
  Config
    { helpMessage :: Text
    , userPrefsDir :: FilePath
    , repeat :: RepeatConf
    , logLevel :: String
    , telegram :: TelegramConf
    }
  deriving (Eq, Show)

data RepeatConf =
  RepeatConf
    { message :: Text
    , defaultCount :: Int
    }
  deriving (Eq, Show)

data TelegramConf =
  TelegramConf
    { pollTimeout :: Int
    , apiKey :: Text
    }
  deriving (Eq, Show)

instance FromJSON Config where
  parseJSON =
    withObject "Config" $ \obj ->
      Config <$> obj .:? "help_message" .!= "Echo Bot" <*>
      obj .:? "user_prefs_dir" .!= "~/.chache/echobot" <*>
      obj .: "repeat" <*>
      obj .:? "log_level" .!= "info" <*>
      obj .: "telegram"

instance FromJSON RepeatConf where
  parseJSON =
    withObject "RepeatConf" $ \v ->
      RepeatConf <$> v .:? "message" .!= "Set repeat count" <*>
      v .:? "default_count" .!= 1

instance FromJSON TelegramConf where
  parseJSON =
    withObject "TelegramConf" $ \v ->
      TelegramConf <$> v .:? "poll_timeout" .!= 0 <*> v .:? "api_key" .!= mempty

configFile :: FilePath
configFile = "config.yaml"

defaultConfig :: Config
defaultConfig =
  case parse parseJSON emptyObject of
    Success config -> config
    _ -> error "Fatal error: could not build default configuration"

readConfig :: IO Config
readConfig = do
  result <- decodeFileEither configFile
  case result of
    Right config -> return config
    Left exception -> do
      warningM rootLoggerName $ displayException exception
      warningM
        rootLoggerName
        "Something seems to be wrong with reading configuration file. Using default configuration."
      return defaultConfig
