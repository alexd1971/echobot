{-# LANGUAGE DeriveFunctor #-}

module DSL.BotLang where

import Config (RepeatConf(message))
import Control.Monad.Free
import Data.Aeson

data Update
  = Message {value :: Value}
  | Command {value :: Value}
  | CallBackData {value :: Value}
  deriving (Show)

data BotLang a
  = GetUpdates ([Update] -> a)
  | ExecuteCommand Update a
  | EchoMessage Update a
  | SetUserPreferences Update a
  deriving (Functor)

type BotScript = Free BotLang

getUpdates :: BotScript [Update]
getUpdates = Free (GetUpdates Pure)

executeCommand :: Update -> BotScript ()
executeCommand command = Free (ExecuteCommand command $ Pure ())

echoMessage :: Update -> BotScript ()
echoMessage message = Free (EchoMessage message $ Pure ())

setUserPreferences :: Update -> BotScript ()
setUserPreferences prefs = Free (SetUserPreferences prefs $ Pure ())

class Monad m =>
      Interpreter m
  where
  onGetUpdates :: m [Update]
  onExecuteCommand :: Update -> m ()
  onEchoMessage :: Update -> m ()
  onSetUserPreferences :: Update -> m ()

interpret :: Interpreter m => BotScript a -> m a
interpret (Pure a) = pure a
interpret (Free (GetUpdates next)) = do
  updates <- onGetUpdates
  interpret (next updates)
interpret (Free (ExecuteCommand command next)) = do
  onExecuteCommand command
  interpret next
interpret (Free (EchoMessage message next)) = do
  onEchoMessage message
  interpret next
interpret (Free (SetUserPreferences prefs next)) = do
  onSetUserPreferences prefs
  interpret next
