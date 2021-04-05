{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Voice where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)

data Voice = Voice
  { fileId :: String,
    fileUniqueId :: String,
    duration :: Integer,
    mimeType :: Maybe String,
    fileSize :: Maybe Integer
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Voice
 )
