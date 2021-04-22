{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Invoice where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics (Generic)

data Invoice = Invoice
  { title :: String,
    description :: String,
    startParameter :: String,
    currency :: String,
    totalAmount :: Integer
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Invoice
 )
