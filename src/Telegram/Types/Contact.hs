{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Contact where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)

data Contact = Contact
  { phoneNumber :: String,
    firstName :: String,
    lastName :: Maybe String,
    userId :: Maybe Integer,
    vcard :: Maybe String
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Contact
 )
