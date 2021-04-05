{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.MessageAutoDeleteTimerChanged where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)

newtype MessageAutoDeleteTimerChanged = MessageAutoDeleteTimerChanged
  { messageAutoDeleteTime :: Integer
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''MessageAutoDeleteTimerChanged
 )
