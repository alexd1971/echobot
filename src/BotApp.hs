{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BotApp where

import Config (Config(logLevel))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Logger
  ( LogLevel
  , LogSource
  , LoggingT(runLoggingT)
  , MonadLogger
  , filterLogger
  , runStdoutLoggingT
  )
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT(runReaderT))
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.HashMap.Lazy (HashMap)
import Logs (logLevelFilter, logOutput)

type UserPrefs = HashMap Int Int

data BotState =
  BotState
    { offset :: Int
    , prefs :: UserPrefs
    }

newtype BotApp a =
  BotApp
    { run :: ReaderT Config (StateT BotState (LoggingT IO)) a
    }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadCatch
           , MonadIO
           , MonadLogger
           , MonadMask
           , MonadReader Config
           , MonadState BotState
           , MonadThrow
           )

runBotApp :: BotApp a -> Config -> BotState -> IO a
runBotApp app config initialState =
  runLoggingT
    (filterLogger
       (logLevelFilter $ logLevel config)
       (evalStateT (runReaderT (run app) config) initialState))
    logOutput
