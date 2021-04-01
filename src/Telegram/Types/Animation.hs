module Telegram.Types.Animation where

import Telegram.Types.PhotoSize (PhotoSize)

data Animation = Animation
  {
    fileId :: String,
    fileUniqueId :: String,
    width :: Integer,
    height :: Integer,
    duration :: Integer,
    thumb :: Maybe PhotoSize,
    fileName :: Maybe String,
    mimeType :: Maybe String,
    fileSize :: Maybe Integer
  }
