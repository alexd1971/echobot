module Telegram.Types.Audio where

import Telegram.Types.PhotoSize (PhotoSize)

data Audio = Audio
  { fileId :: String,
    fileUniqueId :: String,
    duration :: Integer,
    performer :: Maybe String,
    title :: Maybe String,
    fileName :: Maybe String,
    mimeType :: Maybe String,
    fileSize :: Maybe Integer,
    thumb :: Maybe PhotoSize
  }
