{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Dice where

import Data.Aeson.TH (defaultOptions, deriveJSON)

data Dice = Dice
  { emoji :: String,
    value :: Integer
  }
  deriving (Show)

$(deriveJSON defaultOptions ''Dice)
