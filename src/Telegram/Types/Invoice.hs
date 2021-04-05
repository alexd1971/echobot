{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Invoice where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)

data Invoice = Invoice
  { title :: String,
    description :: String,
    startParameter :: String,
    currency :: String,
    totalAmount :: Integer
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Invoice
 )
