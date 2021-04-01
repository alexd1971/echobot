module Telegram.Types.MaskPosition where

data MaskPosition = MaskPosition
  { point :: String,
    xShift :: Float,
    yShift :: Float,
    scale :: Float
  }
