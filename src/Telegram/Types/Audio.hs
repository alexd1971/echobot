{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Audio where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics
import Telegram.Types.PhotoSize (PhotoSize)

data Audio = Audio
  { fileId :: String,
    fileUniqueId :: String,
    duration :: Integer,
    performer :: Maybe String,
    title :: Maybe String,
    fileName :: Maybe String,
    mimeType :: Maybe String,
    fileSize :: Maybe Integer,
    thumb :: Maybe PhotoSize
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Audio
 )
