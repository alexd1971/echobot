module Telegram.Types.EncryptedPassportElement where

import Telegram.Types.PassportFile (PassportFile)

data EncryptedPassporElement = EncryptedPassporElement
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
