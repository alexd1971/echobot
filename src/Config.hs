{-# LANGUAGE OverloadedStrings #-}

module Config
  ( RepeatConf(..)
  , TelegramConf(..)
  , Config(..)
  , readConfig
  ) where

import Control.Exception (Exception(displayException))
import Control.Monad.Logger
  ( LogLevel(LevelDebug, LevelError, LevelInfo, LevelWarn)
  )
import Data.Aeson
  ( FromJSON(parseJSON)
  , Result(Success)
  , (.!=)
  , (.:)
  , (.:?)
  , withObject
  )
import Data.Aeson.Types (emptyObject, parse, parseMaybe, withText)
import Data.Maybe (fromMaybe)
import Data.Text (Text, toLower)
import Data.Yaml (decodeFileEither, decodeFileThrow)
import System.Directory (doesFileExist)

data Config =
  Config
    { helpMessage :: Text
    , userPrefsDir :: FilePath
    , repeat :: RepeatConf
    , logLevel :: LogLevel
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

instance FromJSON LogLevel where
  parseJSON =
    withText "Log Level" $ \v ->
      case toLower v of
        "debug" -> pure LevelDebug
        "info" -> pure LevelInfo
        "warn" -> pure LevelWarn
        "error" -> pure LevelError
        _ -> fail "Unknown Log Level"

instance FromJSON Config where
  parseJSON =
    withObject "Config" $ \obj ->
      Config <$> obj .:? "help_message" .!= "Echo Bot" <*>
      obj .:? "user_prefs_dir" .!= "~/.cache/echobot" <*>
      obj .: "repeat" <*>
      obj .:? "log_level" .!= LevelInfo <*>
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
  configFileExists <- doesFileExist configFile
  if configFileExists
    then decodeFileThrow configFile
    else return defaultConfig
