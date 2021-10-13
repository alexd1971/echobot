{-# LANGUAGE OverloadedStrings #-}

module JSONParsers where

import Control.Applicative ((<|>), empty)
import Control.Monad ((>=>))
import DSL.BotLang (Update(..))
import Data.Aeson
import Data.Aeson.Types (Parser, Value)
import qualified Data.HashMap.Lazy as HM
import Data.Text (Text)
import qualified Data.Text as T

getUpdates :: Value -> Parser [Update]
getUpdates v = do
  u <- (withObject "Response body" (.: "result") >=> parseJSONList) v
  mapM (\o -> callBackData o <|> command o <|> message o) u

callBackData :: Value -> Parser Update
callBackData update = do
  hasCallBackData <-
    withObject "Update" (pure . HM.member "callback_query") update
  if hasCallBackData
    then return $ CallBackData update
    else empty

command :: Value -> Parser Update
command update = do
  entities <- getEntities update
  if null entities
    then empty
    else do
      let entity = head entities
      eType <- getEntityType entity
      offset <- getEntityOffset entity
      eLength <- getEntityLength entity
      tLength <- T.length <$> getMessageText update
      if eType == "bot_command" &&
         offset == 0 && eLength == fromIntegral tLength
        then return $ Command update
        else empty

message :: Value -> Parser Update
message update = do
  hasMessage <- withObject "Update" (pure . HM.member "message") update
  if hasMessage
    then return $ Message update
    else empty

getCallBackData :: Value -> Parser String
getCallBackData =
  withObject "Update" (.: "callback_query") >=>
  withObject "CallBackQuery" (.: "data")

getMessageText :: Value -> Parser Text
getMessageText = getMessage >=> withObject "Message" (.: "text")

getChatId :: Value -> Parser Int
getChatId =
  getMessage >=>
  withObject "Message" (.: "chat") >=> withObject "Chat" (.: "id")

getUpdateId :: Value -> Parser Int
getUpdateId = withObject "Update" (.: "update_id")

getMessage :: Value -> Parser Value
getMessage = withObject "Update" (.: "message")

getMessageId :: Value -> Parser Int
getMessageId = getMessage >=> withObject "Message" (.: "message_id")

getCallBackQuery :: Value -> Parser Value
getCallBackQuery = withObject "Update" (.: "callback_query")

getId :: Value -> Parser Int
getId =
  withObject "Message or CallBackQuery" (.: "from") >=>
  withObject "from" (.: "id")

getUserId :: Value -> Parser Int
getUserId v = (getMessage v <|> getCallBackQuery v) >>= getId

getEntities :: Value -> Parser [Value]
getEntities =
  getMessage >=> withObject "Message" ((.: "entities") >=> parseJSONList)

getEntityType :: Value -> Parser Text
getEntityType = withObject "MessageEntity" (.: "type")

getEntityOffset :: Value -> Parser Int
getEntityOffset = withObject "MessageEntity" (.: "offset")

getEntityLength :: Value -> Parser Int
getEntityLength = withObject "MessageEntity" (.: "length")
