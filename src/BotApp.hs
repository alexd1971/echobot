{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BotApp where

import Config (Config)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT(runReaderT))
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.HashMap.Lazy (HashMap)
import GHC.Base (Alternative)

type UserPrefs = HashMap Int Int

data BotState =
  BotState
    { offset :: Int
    , prefs :: UserPrefs
    }

newtype BotApp a =
  BotApp
    { run :: ReaderT Config (StateT BotState IO) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Config
           , MonadState BotState
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

runBotApp :: BotApp a -> Config -> BotState -> IO a
runBotApp app config = evalStateT (runReaderT (run app) config)
