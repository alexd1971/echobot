{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

import Test.Hspec (hspec)
import Test.Telegram.Types.PhotoSize ( testPhotoSize )
import Test.Telegram.Types.Animation ( testAnimation )

main :: IO ()
main = hspec $ do
  testPhotoSize
  testAnimation
