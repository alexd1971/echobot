module Telegram.Types.VideoNote where

import Telegram.Types.PhotoSize ( PhotoSize )

data VideoNote = VideoNote
  { fileId :: String,
    fileUniqueId :: String,
    length :: Integer,
    duration :: Integer,
    thumb :: Maybe PhotoSize,
    fileSize :: Maybe Integer
  }
