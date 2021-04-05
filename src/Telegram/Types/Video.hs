{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Video where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import Telegram.Types.PhotoSize (PhotoSize)

data Video = Video
  { fileId :: String,
    fileUniqueId :: String,
    width :: Integer,
    height :: Integer,
    duration :: Integer,
    thumb :: Maybe PhotoSize,
    fileName :: Maybe String,
    mimeType :: Maybe String,
    fileSize :: Maybe Integer
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Video
 )
