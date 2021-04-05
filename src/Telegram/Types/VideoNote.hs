{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.VideoNote where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import Telegram.Types.PhotoSize (PhotoSize)

data VideoNote = VideoNote
  { fileId :: String,
    fileUniqueId :: String,
    length :: Integer,
    duration :: Integer,
    thumb :: Maybe PhotoSize,
    fileSize :: Maybe Integer
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''VideoNote
 )
