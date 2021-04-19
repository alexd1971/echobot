{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.ChatLocation where

import Data.Aeson.TH
  ( Options (omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import GHC.Generics (Generic)
import Telegram.Types.Location (Location)

data ChatLocation = ChatLocation
  { location :: Location,
    address :: String
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { omitNothingFields = True
       }
     ''ChatLocation
 )
