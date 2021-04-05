{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.PassportFile where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)

data PassportFile = PassportFile
  { fileId :: String,
    fileUniqueId :: String,
    fileSize :: Integer,
    fileDate :: Integer
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''PassportFile
 )
