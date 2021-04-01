module Telegram.Types.ProximityAlertTriggered where

import Telegram.Types.User (User)

data ProximityAlertTriggered = ProximityAlertTriggered
  { traveler :: User,
    watcher :: User,
    distance :: Integer
  }
