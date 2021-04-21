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
import Test.Telegram.Types.Dice (testDice)
import Test.Telegram.Types.Document (testDocument)
import Test.Telegram.Types.EncryptedCredentials (testEncryptedCredentials)
import Test.Telegram.Types.EncryptedPassportElement (testEncryptedPassportElement)
import Test.Telegram.Types.Game (testGame)
import Test.Telegram.Types.Location (testLocation)
import Test.Telegram.Types.LoginUrl (testLoginUrl)
import Test.Telegram.Types.MessageEntity (testMessageEntity)
import Test.Telegram.Types.PassportFile (testPassportFile)
import Test.Telegram.Types.PhotoSize (testPhotoSize)
import Test.Telegram.Types.User (testUser)

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
    testDice
    testDocument
    testEncryptedCredentials
    testPassportFile
    testEncryptedPassportElement
    testUser
    testMessageEntity
    testGame
    testLoginUrl
