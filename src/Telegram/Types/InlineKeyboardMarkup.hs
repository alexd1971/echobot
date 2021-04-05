{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.InlineKeyboardMarkup where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import Telegram.Types.InlineKeyboardButton

newtype InlineKeyboardMarkup = InlineKeyboardMarkup
  { inlineKeyboard :: [[InlineKeyboardButton]]
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''InlineKeyboardMarkup
 )
