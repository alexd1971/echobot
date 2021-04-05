{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.MessageEntity where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Telegram.Types.User (User)

data MessageEntity = MessageEntity
  { messageEntityType :: String,
    offset :: Integer,
    length :: Integer,
    url :: Maybe String,
    user :: Maybe User,
    language :: Maybe String
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "messageEntityType" -> "type"
           a -> a,
         omitNothingFields = True
       }
     ''MessageEntity
 )
