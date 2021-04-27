{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.CallbackQuery where

import Data.Aeson
  ( Options (fieldLabelModifier, omitNothingFields),
    camelTo2,
    defaultOptions,
  )
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import Telegram.Types.ChatMessage (Message)
import Telegram.Types.User (User)

data CallbackQuery = CallbackQuery
  { queryId :: String,
    from :: User,
    message :: Maybe Message,
    inlineMessageId :: Maybe String,
    chatInstance :: String,
    queryData :: Maybe String,
    gameShortName :: Maybe String
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "queryId" -> "id"
           "queryData" -> "data"
           a -> camelTo2 '_' a,
         omitNothingFields = True
       }
     ''CallbackQuery
 )
