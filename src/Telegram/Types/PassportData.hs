{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.PassportData where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import Telegram.Types.EncryptedCredentials (EncryptedCredentials)
import Telegram.Types.EncryptedPassportElement
  ( EncryptedPassportElement,
  )

data PassportData = PassportData
  { passportData :: [EncryptedPassportElement],
    credentials :: EncryptedCredentials
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''PassportData
 )
