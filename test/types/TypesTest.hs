{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

import Test.Hspec (hspec)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.Telegram.Types.Animation (testAnimation)
import Test.Telegram.Types.Audio (testAudio)
import Test.Telegram.Types.ChatLocation (testChatLocation)
import Test.Telegram.Types.ChatPermissions (testChatPermissions)
import Test.Telegram.Types.Contact (testContact)
import Test.Telegram.Types.Location (testLocation)
import Test.Telegram.Types.PhotoSize (testPhotoSize)

main :: IO ()
main = hspec $ do
  modifyMaxSuccess (const 10) $ do
    testPhotoSize
    testAnimation
    testAudio
    testLocation
    testChatLocation
    testChatPermissions
    testContact
