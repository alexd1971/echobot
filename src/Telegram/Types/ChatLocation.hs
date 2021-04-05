{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.ChatLocation where

import Data.Aeson.TH
  ( Options (omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Telegram.Types.Location

data ChatLocation = ChatLocation
  { location :: Location,
    address :: String
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { omitNothingFields = True
       }
     ''ChatLocation
 )
