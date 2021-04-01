module Telegram.Types.Video where

import Telegram.Types.PhotoSize (PhotoSize)

data Video = Video
  { fileId :: String,
    fileUniqueId :: String,
    width :: Integer,
    height :: Integer,
    duration :: Integer,
    thumb :: Maybe PhotoSize,
    fileName :: Maybe String,
    mimeType :: Maybe String,
    fileSize :: Maybe Integer
  }
