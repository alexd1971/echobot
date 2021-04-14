{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

import Test.Hspec (hspec)
import Test.Telegram.Types.PhotoSize ( testPhotoSize )

main :: IO ()
main = hspec $ do
  testPhotoSize
