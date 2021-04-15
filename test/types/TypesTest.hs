{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

import Test.Hspec (hspec)
import Test.Telegram.Types.Animation (testAnimation)
import Test.Telegram.Types.Audio (testAudio)
import Test.Telegram.Types.PhotoSize (testPhotoSize)
import Test.Hspec.QuickCheck (modifyMaxSuccess)

main :: IO ()
main = hspec $ do
  modifyMaxSuccess (const 10) $ do
    testPhotoSize
    testAnimation
    testAudio
