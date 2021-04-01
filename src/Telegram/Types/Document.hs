module Telegram.Types.Document where

import Telegram.Types.PhotoSize (PhotoSize)

data Document = Document
  { fileId :: String,
    fileUniqueId :: String,
    thumb :: Maybe PhotoSize,
    fileName :: Maybe String,
    mimeType :: Maybe String,
    fileSize :: Maybe Integer
  }
