module BotEnv where

import Config
import Control.Monad.Reader
import Control.Monad.State
import Data.HashMap.Lazy

type UserPrefs = HashMap Int Int
data BotState = BotState {offset :: Int, prefs :: UserPrefs}
type BotEnv = ReaderT Config (StateT BotState IO)

runBotEnv :: BotEnv a -> Config -> BotState -> IO a
runBotEnv env = evalStateT . runReaderT env
