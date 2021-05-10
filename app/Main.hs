module Main where

import Config (readConfig, runConfigReader)
import Logging (initLogger)
import qualified Logging as Log

main :: IO ()
main = do
  config <- readConfig
  runConfigReader initLogger config
  Log.info "Bot started."
