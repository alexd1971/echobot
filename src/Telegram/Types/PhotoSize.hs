{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.PhotoSize where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)

data PhotoSize = PhotoSize
  { fileId :: String,
    fileUniqueId :: String,
    width :: Integer,
    height :: Integer,
    fileSize :: Maybe Integer
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''PhotoSize
 )
