module Main where

import Logging (initLogging)
import System.Log.Logger (infoM, rootLoggerName)
import Control.Monad.Reader
import Config

main :: IO ()
main = do
  config <- readConfig
  runReaderT initLogging config
  infoM rootLoggerName "Bot started."
