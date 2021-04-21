{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.InlineKeyboardButton where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2, emptyObject)
import GHC.Generics (Generic)
import Telegram.Types.LoginUrl (LoginUrl)

data CallbackGame = CallbackGame deriving (Eq, Show, Generic)

instance FromJSON CallbackGame where
  parseJSON _ = do return CallbackGame

instance ToJSON CallbackGame where
  toJSON _ = emptyObject

data InlineKeyboardButton = InlineKeyboardButton
  { text :: String,
    url :: Maybe String,
    loginUrl :: Maybe LoginUrl,
    callbackData :: Maybe String,
    switchInlineQuery :: Maybe String,
    switchInlineQueryCurrentChat :: Maybe String,
    callbackGame :: Maybe CallbackGame,
    pay :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''InlineKeyboardButton
 )
