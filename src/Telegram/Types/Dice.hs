{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Dice where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import GHC.Generics (Generic)

data Dice = Dice
  { emoji :: String,
    value :: Integer
  }
  deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Dice)
