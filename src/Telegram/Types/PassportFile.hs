module Telegram.Types.PassportFile where

data PassportFile = PassportFile
  { fileId :: String,
    fileUniqueId :: String,
    fileSize :: Integer,
    fileDate :: Integer
  }
