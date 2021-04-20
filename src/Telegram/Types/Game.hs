{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Game where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics (Generic)
import Telegram.Types.Animation (Animation)
import Telegram.Types.MessageEntity (MessageEntity)
import Telegram.Types.PhotoSize (PhotoSize)

data Game = Game
  { title :: String,
    description :: String,
    photo :: [PhotoSize],
    text :: Maybe String,
    textEntities :: Maybe [MessageEntity],
    animation :: Maybe Animation
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Game
 )
