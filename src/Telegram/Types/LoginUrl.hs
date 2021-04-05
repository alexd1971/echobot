{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.LoginUrl where

import Data.Aeson.Types
import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )

data LoginUrl = LoginUrl
  { url :: String,
    forwardText :: Maybe String,
    botUsername :: Maybe String,
    requestWriteAccess :: Maybe Bool
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''LoginUrl
 )
