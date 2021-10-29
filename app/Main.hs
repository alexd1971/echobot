module Main where

import Bot (botScript)
import BotApp (BotApp(BotApp), BotState(..), runBotApp)
import Config (readConfig)
import Control.Monad (forever)
import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT(runReaderT))
import GHC.Conc.IO (threadDelay)
import Interpreter.Telegram (runBotScript)
import Network.HTTP.Req (HttpException(JsonHttpException, VanillaHttpException))
import UserPrefs (cacheUserPrefs)

main :: IO ()
main = do
  config <- readConfig
  prefs <- runReaderT cacheUserPrefs config
  putStrLn "Telegram Bot"
  runBotApp
    (forever $ runBotScript botScript `catch` handler)
    config
    (BotState 0 prefs)
  where
    handler :: HttpException -> BotApp ()
    handler (VanillaHttpException e) = do
      liftIO $ print e
      liftIO $ threadDelay tenSeconds
    handler e = do
      liftIO $ print e
    tenSeconds = 10000000
