{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.PollOption where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics (Generic)

data PollOption = PollOption
  { text :: String,
    voterCount :: Integer
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''PollOption
 )
