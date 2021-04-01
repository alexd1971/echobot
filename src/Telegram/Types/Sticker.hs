module Telegram.Types.Sticker where

import Telegram.Types.MaskPosition (MaskPosition)
import Telegram.Types.PhotoSize (PhotoSize)

data Sticker = Sticker
  { fileId :: String,
    fileUniqueId :: String,
    width :: Integer,
    height :: Integer,
    isAnimated :: Bool,
    thumb :: Maybe PhotoSize,
    emoji :: Maybe String,
    setName :: Maybe String,
    maskPosition :: Maybe MaskPosition,
    fileSize :: Maybe Integer
  }
