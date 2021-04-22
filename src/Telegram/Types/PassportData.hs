{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.PassportData where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics (Generic)
import Telegram.Types.EncryptedCredentials (EncryptedCredentials)
import Telegram.Types.EncryptedPassportElement (EncryptedPassportElement)

data PassportData = PassportData
  { passportData :: [EncryptedPassportElement],
    credentials :: EncryptedCredentials
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "passportData" -> "data"
           a -> camelTo2 '_' a,
         omitNothingFields = True
       }
     ''PassportData
 )
