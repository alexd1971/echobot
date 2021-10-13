{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Interpreter.Telegram
  ( runBotScript
  ) where

import BotEnv (BotEnv, BotState(BotState))
import qualified BotEnv as Env
import qualified Config as C
import Control.Applicative ((<|>))
import Control.Monad (replicateM, unless, when)
import Control.Monad.Reader (asks, lift, liftIO)
import Control.Monad.State (gets, modify, put)
import DSL.BotLang (BotScript, Interpreter(..), Update(..), interpret)
import Data.Aeson
  ( FromJSON(parseJSONList)
  , KeyValue((.=))
  , ToJSON(toJSONList)
  , Value
  , (.:)
  , object
  , withObject
  )
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust, fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import GHC.IO (throwIO)
import JSONParsers
import Network.HTTP.Req
  ( GET(GET)
  , MonadHttp(handleHttpException)
  , NoReqBody(NoReqBody)
  , POST(POST)
  , ReqBodyJson(ReqBodyJson)
  , Scheme(Https)
  , Url
  , (/:)
  , (=:)
  , https
  , ignoreResponse
  , jsonResponse
  , req
  , responseBody
  )
import System.FilePath ((</>))
import Text.Read (readMaybe)

instance MonadHttp BotEnv where
  handleHttpException = liftIO . throwIO

instance Interpreter BotEnv where
  onGetUpdates = do
    telegramApi <- getTelegramApi
    timeout <- asks (C.pollTimeout . C.telegram)
    offset <- lift $ gets Env.offset
    body <-
      responseBody <$>
      req
        GET
        (telegramApi /: "getUpdates")
        NoReqBody
        jsonResponse
        ("offset" =: offset <> "timeout" =: timeout)
    let updates = fromMaybe [] (parseMaybe getUpdates body)
    unless (null updates) $ do
      let updateId =
            fromMaybe 0 (parseMaybe getUpdateId (value . last $ updates))
      when (updateId > 0) (lift $ modify $ updateOffset updateId)
    return updates
  onExecuteCommand (Command v) = do
    let command = fromMaybe "" (parseMaybe getMessageText v)
        chatId = fromMaybe 0 (parseMaybe getChatId v)
    when (command /= "" && chatId /= 0) $ do
      case command of
        "/help" -> showHelp chatId
        "/repeat" -> showRepeat chatId
        _ -> unknownCommand chatId
  onExecuteCommand _ = return ()
  onEchoMessage (Message v) = do
    let chatId = fromMaybe 0 (parseMaybe getChatId v)
        messageId = fromMaybe 0 (parseMaybe getMessageId v)
        userId = fromMaybe 0 (parseMaybe getUserId v)
    when (chatId /= 0 && messageId /= 0 && userId /= 0) $ do
      prefs <- gets Env.prefs
      defaultCount <- asks (C.defaultCount . C.repeat)
      let count = fromJust $ HM.lookup userId prefs <|> pure defaultCount
          reqBody =
            object
              [ "chat_id" .= chatId
              , "from_chat_id" .= chatId
              , "message_id" .= messageId
              ]
      telegramApi <- getTelegramApi
      rb <-
        replicateM
          count
          (responseBody <$>
           req
             POST
             (telegramApi /: "copyMessage")
             (ReqBodyJson reqBody)
             jsonResponse
             mempty :: BotEnv Value)
      return ()
  onEchoMessage _ = return ()
  onSetUserPreferences (CallBackData v) = do
    let count = fromMaybe 0 $ parseMaybe getCallBackData v >>= readMaybe
        userId = fromMaybe 0 (parseMaybe getUserId v)
    when (count /= 0 && userId /= 0) $ do
      lift $ modify $ setUserPref userId count
      prefDir <- asks C.userPrefsDir
      liftIO $ writeFile (prefDir </> show userId) (show count)
  onSetUserPreferences _ = return ()

runBotScript :: BotScript a -> BotEnv a
runBotScript = interpret

updateOffset :: Int -> BotState -> BotState
updateOffset updateId (BotState _ prefs) = BotState (updateId + 1) prefs

setUserPref :: Int -> Int -> BotState -> BotState
setUserPref userId count botState =
  let prefs = Env.prefs botState
   in botState {Env.prefs = HM.insert userId count prefs}

getTelegramApi :: BotEnv (Url 'Https)
getTelegramApi = do
  apiKey <- asks (C.apiKey . C.telegram)
  return $ https "api.telegram.org" /: ("bot" <> apiKey)

showHelp :: Int -> BotEnv ()
showHelp chatId = do
  telegramApi <- getTelegramApi
  text <- asks C.helpMessage
  let message = object ["chat_id" .= chatId, "text" .= text]
  req
    POST
    (telegramApi /: "sendMessage")
    (ReqBodyJson message)
    ignoreResponse
    mempty
  return ()

showRepeat :: Int -> BotEnv ()
showRepeat chatId = do
  text <- asks (C.message . C.repeat)
  telegramApi <- getTelegramApi
  let message =
        object
          [ "chat_id" .= chatId
          , "text" .= text
          , "reply_markup" .=
            object
              [ "inline_keyboard" .=
                toJSONList
                  [ [ object
                        [ "text" .= ("1" :: String)
                        , "callback_data" .= ("1" :: String)
                        ]
                    , object
                        [ "text" .= ("2" :: String)
                        , "callback_data" .= ("2" :: String)
                        ]
                    , object
                        [ "text" .= ("3" :: String)
                        , "callback_data" .= ("3" :: String)
                        ]
                    , object
                        [ "text" .= ("4" :: String)
                        , "callback_data" .= ("4" :: String)
                        ]
                    , object
                        [ "text" .= ("5" :: String)
                        , "callback_data" .= ("5" :: String)
                        ]
                    ]
                  ]
              ]
          ]
  req
    POST
    (telegramApi /: "sendMessage")
    (ReqBodyJson message)
    ignoreResponse
    mempty
  return ()

unknownCommand :: Int -> BotEnv ()
unknownCommand chatId = do
  telegramApi <- getTelegramApi
  let message =
        object ["chat_id" .= chatId, "text" .= ("Unknown command" :: Text)]
  req
    POST
    (telegramApi /: "sendMessage")
    (ReqBodyJson message)
    ignoreResponse
    mempty
  return ()
