{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Document where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics (Generic)
import Telegram.Types.PhotoSize (PhotoSize)

data Document = Document
  { fileId :: String,
    fileUniqueId :: String,
    thumb :: Maybe PhotoSize,
    fileName :: Maybe String,
    mimeType :: Maybe String,
    fileSize :: Maybe Integer
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Document
 )
