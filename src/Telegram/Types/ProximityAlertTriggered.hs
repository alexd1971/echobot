{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.ProximityAlertTriggered where

import Data.Aeson.TH
  ( Options (omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import GHC.Generics (Generic)
import Telegram.Types.User (User)

data ProximityAlertTriggered = ProximityAlertTriggered
  { traveler :: User,
    watcher :: User,
    distance :: Integer
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { omitNothingFields = True
       }
     ''ProximityAlertTriggered
 )
