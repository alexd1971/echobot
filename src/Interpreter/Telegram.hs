{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Telegram
  ( runBotScript
  ) where

import BotApp (BotApp, BotState(BotState))
import qualified BotApp as AppState (offset, prefs)
import qualified Config as C
import Control.Applicative ((<|>))
import Control.Monad (replicateM, unless, when)
import Control.Monad.Catch
import Control.Monad.Logger (logDebugN, logInfoN)
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
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Text (pack)
import Data.Text.Lazy (Text, toStrict)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (toLazyText)
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
  , responseBody, renderUrl
  )
import System.FilePath ((</>))
import Text.Read (readMaybe)

instance MonadHttp BotApp where
  handleHttpException = throwM

instance Interpreter BotApp
  -- Gets updates
                  where
  onGetUpdates = do
    logInfoN "Waiting for updates..."
    telegramApi <- getTelegramApi
    timeout <- asks (C.pollTimeout . C.telegram)
    offset <- gets AppState.offset
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
      let updateIds = mapMaybe (parseMaybe getUpdateId . value) updates
          lastUpdateId = last updateIds
      logInfoN $ "Got updates: " <> (pack . show $ updateIds)
      logDebugN $ toStrict . toLazyText . encodePrettyToTextBuilder $ body
      modify $ updateOffset lastUpdateId
    when (null updates) $ logInfoN "Timed out."
    return updates
  -- Executes command
  onExecuteCommand (Command v) = do
    logInfoN "Executing command..."
    logDebugN $ toStrict . toLazyText . encodePrettyToTextBuilder $ v
    let command = fromMaybe "" (parseMaybe getMessageText v)
        chatId = fromMaybe 0 (parseMaybe getChatId v)
        userId = fromMaybe 0 (parseMaybe getUserId v)
    when (command /= "" && chatId /= 0) $ do
      logInfoN $
        "Received command: " <>
        command <>
        ", chat Id: " <>
        (pack . show) chatId <> ", user Id: " <> (pack . show) userId
      case command of
        "/help" -> showHelp chatId
        "/repeat" -> showRepeat chatId
        _ -> unknownCommand chatId
  onExecuteCommand _ = return ()
  -- Echo Message
  onEchoMessage (Message v) = do
    let chatId = fromMaybe 0 (parseMaybe getChatId v)
        messageId = fromMaybe 0 (parseMaybe getMessageId v)
        userId = fromMaybe 0 (parseMaybe getUserId v)
    when (chatId /= 0 && messageId /= 0 && userId /= 0) $ do
      logInfoN $
        "Received message " <>
        (pack . show) messageId <> " from user " <> (pack . show) userId
      prefs <- gets AppState.prefs
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
             mempty :: BotApp Value)
      return ()
  onEchoMessage _ = return ()
  onSetUserPreferences (CallBackData v) = do
    liftIO $ putStrLn "Setup user preferences:"
    let count = fromMaybe 0 $ parseMaybe getCallBackData v >>= readMaybe
        userId = fromMaybe 0 (parseMaybe getUserId v)
    when (count /= 0 && userId /= 0) $ do
      modify $ setUserPref userId count
      prefDir <- asks C.userPrefsDir
      liftIO $ writeFile (prefDir </> show userId) (show count)
  onSetUserPreferences _ = return ()

runBotScript :: BotScript a -> BotApp a
runBotScript = interpret

updateOffset :: Int -> BotState -> BotState
updateOffset updateId (BotState _ prefs) = BotState (updateId + 1) prefs

setUserPref :: Int -> Int -> BotState -> BotState
setUserPref userId count botState =
  let prefs = AppState.prefs botState
   in botState {AppState.prefs = HM.insert userId count prefs}

getTelegramApi :: BotApp (Url 'Https)
getTelegramApi = do
  apiKey <- asks (C.apiKey . C.telegram)
  return $ https "api.telegram.org" /: ("bot" <> apiKey)

showHelp :: Int -> BotApp ()
showHelp chatId = do
  logInfoN $ "Sending help message to chat " <> (pack . show) chatId <> "."
  telegramApi <- getTelegramApi
  text <- asks C.helpMessage
  let message = object ["chat_id" .= chatId, "text" .= text]
  logDebugN $ "Telegram api: " <> renderUrl telegramApi
  logDebugN $ toStrict . toLazyText . encodePrettyToTextBuilder $ message
  req
    POST
    (telegramApi /: "sendMessage")
    (ReqBodyJson message)
    ignoreResponse
    mempty
  return ()

showRepeat :: Int -> BotApp ()
showRepeat chatId = do
  logInfoN $ "Showing repeat dialog to chat " <> (pack . show) chatId <> "."
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
  logDebugN $ "Telegram api: " <> renderUrl telegramApi
  logDebugN $ toStrict . toLazyText . encodePrettyToTextBuilder $ message
  req
    POST
    (telegramApi /: "sendMessage")
    (ReqBodyJson message)
    ignoreResponse
    mempty
  return ()

unknownCommand :: Int -> BotApp ()
unknownCommand chatId = do
  liftIO $ putStrLn "Show unknown command"
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
