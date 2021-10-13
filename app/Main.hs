module Main where

import Bot (botScript)
import BotEnv (BotState(..), runBotEnv)
import Config (readConfig)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT(runReaderT))
import Interpreter.Telegram (runBotScript)
import UserPrefs (cacheUserPrefs)

main :: IO ()
main = do
  config <- readConfig
  prefs <- runReaderT cacheUserPrefs config
  putStrLn "Telegram Bot"
  runBotEnv (forever $ runBotScript botScript) config (BotState 0 prefs)
