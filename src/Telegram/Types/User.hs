{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.User where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)

data User = User
  { userId :: Integer,
    isBot :: Bool,
    firstName :: String,
    lastName :: Maybe String,
    username :: Maybe String,
    languageCode :: Maybe String,
    canJoinGroups :: Maybe Bool,
    canReadAllGroupMessages :: Maybe Bool,
    supportsInlineQueries :: Maybe Bool
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "userId" -> "id"
           a -> camelTo2 '_' a,
         omitNothingFields = True
       }
     ''User
 )
