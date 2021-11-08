{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot (botScript)
import BotApp (BotApp(BotApp), BotState(..), runBotApp)
import Config (readConfig)
import Control.Monad (forever)
import Control.Monad.Catch (MonadCatch(catch))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logErrorN, logInfoN)
import Control.Monad.Reader (ReaderT(runReaderT))
import Data.Text (pack)
import GHC.Conc.IO (threadDelay)
import Interpreter.Telegram (runBotScript)
import Network.HTTP.Req (HttpException(JsonHttpException, VanillaHttpException))
import UserPrefs (cacheUserPrefs)

main :: IO ()
main = do
  config <- readConfig
  prefs <- runReaderT cacheUserPrefs config
  runBotApp app config (BotState 0 prefs)

app :: BotApp ()
app = do
  logInfoN "Telegram bot started."
  forever $ runBotScript botScript `catch` handler
  where
    handler :: HttpException -> BotApp ()
    handler e = do
      logErrorN (pack . show $ e)
      liftIO $ threadDelay tenSeconds
      where
        tenSeconds = 10000000
