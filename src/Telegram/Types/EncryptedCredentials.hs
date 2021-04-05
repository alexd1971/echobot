{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.EncryptedCredentials where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )

data EncryptedCredentials = EncryptedCredentials
  { credentialsData :: String,
    hash :: String,
    secret :: String
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "credentialsData" -> "data"
           a -> a,
         omitNothingFields = True
       }
     ''EncryptedCredentials
 )
