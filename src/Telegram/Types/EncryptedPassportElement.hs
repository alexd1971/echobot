{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.EncryptedPassportElement where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics (Generic)
import Telegram.Types.PassportFile (PassportFile)

data EncryptedPassportElement = EncryptedPassportElement
  { docType :: String,
    docData :: Maybe String,
    phoneNumber :: Maybe String,
    email :: Maybe String,
    files :: Maybe [PassportFile],
    frontSide :: Maybe PassportFile,
    reverseSide :: Maybe PassportFile,
    selfie :: Maybe PassportFile,
    translation :: Maybe [PassportFile],
    hash :: String
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "docData" -> "data"
           "docType" -> "type"
           a -> camelTo2 '_' a,
         omitNothingFields = True
       }
     ''EncryptedPassportElement
 )
