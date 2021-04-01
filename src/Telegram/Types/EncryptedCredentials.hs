module Telegram.Types.EncryptedCredentials where

data EncryptedCredentials = EncryptedCredentials
  { credentialsData :: String,
    hash :: String,
    secret :: String
  }
