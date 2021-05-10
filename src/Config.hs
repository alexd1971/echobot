{-# LANGUAGE OverloadedStrings #-}

module Config
  ( RepeatConf (..),
    TelegramConf (..),
    Config (..),
    ConfigReader,
    runConfigReader,
    readConfig,
  )
where

import Control.Exception
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson
  ( FromJSON (parseJSON),
    Result (Success),
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (emptyObject, parse, parseMaybe)
import Data.Maybe
import Data.Text (Text)
import Data.Yaml (decodeFileEither)
import System.Log.Logger

data RepeatConf = RepeatConf
  { message :: Text,
    defaultCount :: Int
  }
  deriving (Eq, Show)

instance FromJSON RepeatConf where
  parseJSON = withObject "RepeatConf" $ \v ->
    RepeatConf
      <$> v .:? "message" .!= "Set repeat count"
      <*> v .:? "default_count" .!= 1

data TelegramConf = TelegramConf
  { apiUrl :: Text,
    apiKey :: Text
  }
  deriving (Eq, Show)

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

type ConfigReader a = ReaderT Config IO a

runConfigReader :: ConfigReader a -> Config -> IO a
runConfigReader = runReaderT

configFile :: FilePath
configFile = "config.yaml"

defaultConfig :: Config
defaultConfig = case parse parseJSON emptyObject of
  Success config -> config
  _ -> error "Fatal error: could not build default configuration"

readConfig :: IO Config
readConfig = do
  result <- decodeFileEither configFile
  case result of
    Right config -> return config
    Left exception -> do
      warningM rootLoggerName $ displayException exception
      warningM rootLoggerName "Something seems to be wrong with reading configuration file. Using default configuration."
      return defaultConfig
