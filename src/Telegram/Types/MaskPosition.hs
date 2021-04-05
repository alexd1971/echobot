{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.MaskPosition where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)

data MaskPosition = MaskPosition
  { point :: String,
    xShift :: Float,
    yShift :: Float,
    scale :: Float
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''MaskPosition
 )
