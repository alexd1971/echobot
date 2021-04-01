module Telegram.Types.PassportData where

import Telegram.Types.EncryptedCredentials (EncryptedCredentials)
import Telegram.Types.EncryptedPassportElement
  ( EncryptedPassporElement,
  )

data PassportData = PassportData
  { passportData :: [EncryptedPassporElement],
    credentials :: EncryptedCredentials
  }
