{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Game where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import Telegram.Types.Animation
import Telegram.Types.MessageEntity
import Telegram.Types.PhotoSize

data Game = Game
  { title :: String,
    description :: String,
    photo :: [PhotoSize],
    text :: Maybe String,
    textEntities :: Maybe [MessageEntity],
    animation :: Maybe Animation
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Game
 )
