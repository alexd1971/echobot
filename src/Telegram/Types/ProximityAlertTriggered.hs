{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.ProximityAlertTriggered where

import Data.Aeson.TH
  ( Options (omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Telegram.Types.User (User)

data ProximityAlertTriggered = ProximityAlertTriggered
  { traveler :: User,
    watcher :: User,
    distance :: Integer
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { omitNothingFields = True
       }
     ''ProximityAlertTriggered
 )
