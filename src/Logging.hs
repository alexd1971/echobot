module Logging where

import Config
import Control.Monad.Reader
import System.IO
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Handler.Syslog
import System.Log.Logger
import Data.Char (toUpper)

initLogging :: ReaderT Config IO ()
initLogging = do
  liftIO (hSetBuffering stdout NoBuffering)
  liftIO (hSetBuffering stderr NoBuffering)
  config <- ask
  let logFormatter = simpleLogFormatter "$time - [$prio] - $msg"
      prioriry = read . map toUpper $ logLevel config :: Priority
  consoleHandler <- liftIO (flip setFormatter logFormatter <$> streamHandler stdout prioriry)
  liftIO . updateGlobalLogger rootLoggerName $ (setLevel prioriry . setHandlers [consoleHandler])
