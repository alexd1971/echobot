module Logging
  ( initLogger,
    debug,
    info,
    notice,
    warning,
    Logging.error,
    critical,
    alert,
    emergency,
  )
where

import BotEnv
import Control.Monad.Reader
  ( MonadIO (liftIO),
    MonadReader (ask),
  )
import Data.Char (toUpper)
import System.IO
  ( BufferMode (NoBuffering),
    Handle,
    hSetBuffering,
    stderr,
    stdout,
  )
import System.Log.Formatter (LogFormatter, simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (GenericHandler, streamHandler)
import System.Log.Logger
  ( Priority (ERROR),
    alertM,
    criticalM,
    debugM,
    emergencyM,
    errorM,
    infoM,
    noticeM,
    rootLoggerName,
    setHandlers,
    setLevel,
    updateGlobalLogger,
    warningM,
  )
import Config (Config(logLevel))

logFormatter :: LogFormatter a
logFormatter = simpleLogFormatter "$time - [$prio] - $msg"

mkGenericHandler :: Handle -> Priority -> IO (GenericHandler Handle)
mkGenericHandler handle priority = flip setFormatter logFormatter <$> streamHandler handle priority

initLogger :: BotEnv ()
initLogger = do
  config <- ask
  let prioriry = read . map toUpper $ logLevel config :: Priority
  liftIO (hSetBuffering stdout NoBuffering)
  infoHandler <- liftIO $ mkGenericHandler stdout prioriry
  liftIO (hSetBuffering stderr NoBuffering)
  errorHandler <- liftIO $ mkGenericHandler stderr ERROR
  liftIO
    . updateGlobalLogger rootLoggerName
    $ setLevel prioriry
      . setHandlers [infoHandler, errorHandler]

debug :: String -> IO ()
debug = debugM rootLoggerName

info :: String -> IO ()
info = infoM rootLoggerName

notice :: String -> IO ()
notice = noticeM rootLoggerName

warning :: String -> IO ()
warning = warningM rootLoggerName

error :: String -> IO ()
error = errorM rootLoggerName

critical :: String -> IO ()
critical = criticalM rootLoggerName

alert :: String -> IO ()
alert = alertM rootLoggerName

emergency :: String -> IO ()
emergency = emergencyM rootLoggerName
