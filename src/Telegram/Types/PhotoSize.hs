module Telegram.Types.PhotoSize where

data PhotoSize = PhotoSize
  { fileId :: String,
    fileUniqueId :: String,
    width :: Integer,
    height :: Integer,
    fileSize :: Maybe Integer
  }
