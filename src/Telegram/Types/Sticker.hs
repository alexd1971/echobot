{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Sticker where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
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
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Sticker
 )
