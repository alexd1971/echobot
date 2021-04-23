{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.ChatMessage (testChatPhoto, testChat, testMessage) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Generic.Random.Internal.Generic (FieldGen (FieldGen))
import Telegram.Types.Animation (Animation)
import Telegram.Types.Audio (Audio)
import Telegram.Types.ChatLocation (ChatLocation)
import Telegram.Types.ChatMessage
  ( Chat,
    ChatPhoto,
    Message,
    VoiceChatEnded,
    VoiceChatParticipantsInvited,
    VoiceChatStarted,
    defaultMessage,
  )
import Telegram.Types.ChatPermissions (ChatPermissions)
import Telegram.Types.Contact (Contact)
import Telegram.Types.Dice (Dice)
import Telegram.Types.Document (Document)
import Telegram.Types.Game (Game)
import Telegram.Types.InlineKeyboardMarkup (InlineKeyboardMarkup)
import Telegram.Types.Invoice (Invoice)
import Telegram.Types.Location (Location)
import Telegram.Types.MessageAutoDeleteTimerChanged
  ( MessageAutoDeleteTimerChanged,
  )
import Telegram.Types.MessageEntity (MessageEntity)
import Telegram.Types.PassportData (PassportData)
import Telegram.Types.PhotoSize (PhotoSize)
import Telegram.Types.Poll (Poll)
import Telegram.Types.ProximityAlertTriggered
  ( ProximityAlertTriggered,
  )
import Telegram.Types.Sticker (Sticker)
import Telegram.Types.SuccessfulPayment (SuccessfulPayment)
import Telegram.Types.User (User)
import Telegram.Types.Venue (Venue)
import Telegram.Types.Video (Video)
import Telegram.Types.VideoNote (VideoNote)
import Telegram.Types.Voice (Voice)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.Animation
import Test.Telegram.Types.Audio
import Test.Telegram.Types.ChatLocation
import Test.Telegram.Types.ChatPermissions
import Test.Telegram.Types.Contact
import Test.Telegram.Types.Dice
import Test.Telegram.Types.Document
import Test.Telegram.Types.Game
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.InlineKeyboardMarkup
import Test.Telegram.Types.Invoice
import Test.Telegram.Types.MessageAutoDeleteTimerChanged
import Test.Telegram.Types.PassportData
import Test.Telegram.Types.Poll
import Test.Telegram.Types.ProximityAlertTriggered
import Test.Telegram.Types.Sticker
import Test.Telegram.Types.SuccessfulPayment
import Test.Telegram.Types.User
import Test.Telegram.Types.Venue
import Test.Telegram.Types.Video
import Test.Telegram.Types.VideoNote
import Test.Telegram.Types.Voice

instance Arbitrary VoiceChatStarted where
  arbitrary = genericArbitraryU

instance JSONTestable VoiceChatStarted

instance Arbitrary VoiceChatEnded where
  arbitrary = genericArbitraryU

instance JSONTestable VoiceChatEnded

instance Arbitrary VoiceChatParticipantsInvited where
  arbitrary = genericArbitraryU

genNothingMessage :: Gen (Maybe Message)
genNothingMessage = return Nothing

instance Arbitrary ChatPhoto where
  arbitrary = genericArbitraryU

instance JSONTestable ChatPhoto

chatPhotoKeys =
  sort
    [ "small_file_id",
      "small_file_unique_id",
      "big_file_id",
      "big_file_unique_id"
    ]

chatPhotoWithAllKeys :: IO ChatPhoto
chatPhotoWithAllKeys = generate arbitrary

testChatPhoto :: Spec
testChatPhoto = do
  describe "Test ChatPhoto JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty ChatPhoto)
    object <- runIO chatPhotoWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just chatPhotoKeys

instance Arbitrary Chat where
  arbitrary = genericArbitraryUG genNothingMessage

instance JSONTestable Chat

chatKeys =
  sort
    [ "id",
      "type",
      "title",
      "username",
      "first_name",
      "last_name",
      "photo",
      "bio",
      "description",
      "invite_link",
      "pinned_message",
      "permissions",
      "slow_mode_delay",
      "message_auto_delete_time",
      "sticker_set_name",
      "can_set_sticker_set",
      "linked_chat_id",
      "location"
    ]

chatKeyGenerators ::
  Gen (Maybe Integer)
    :+ Gen (Maybe String)
    :+ Gen (Maybe Bool)
    :+ Gen (Maybe ChatPhoto)
    :+ Gen (Maybe Message)
    :+ Gen (Maybe ChatPermissions)
    :+ Gen (Maybe ChatLocation)
chatKeyGenerators =
  genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust

chatWithAllKeys :: IO Chat
chatWithAllKeys = generate $ genericArbitraryUG chatKeyGenerators

testChat :: Spec
testChat = do
  describe "Test Chat JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty Chat)
    object <- runIO chatWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just chatKeys

instance Arbitrary Message where
  arbitrary = genericArbitraryUG genNothingMessage

instance JSONTestable Message

messageKeys =
  sort
    [ "message_id",
      "from",
      "sender_chat",
      "date",
      "chat",
      "forward_from",
      "forward_from_chat",
      "forward_from_message_id",
      "forward_signature",
      "forward_sender_name",
      "forward_date",
      "reply_to_message",
      "via_bot",
      "edit_date",
      "media_group_id",
      "author_signature",
      "text",
      "entities",
      "animation",
      "audio",
      "document",
      "photo",
      "sticker",
      "video",
      "video_note",
      "voice",
      "caption",
      "caption_entities",
      "contact",
      "dice",
      "game",
      "poll",
      "venue",
      "location",
      "new_chat_members",
      "left_chat_member",
      "new_chat_title",
      "new_chat_photo",
      "delete_chat_photo",
      "group_chat_created",
      "supergroup_chat_created",
      "channel_chat_created",
      "message_auto_delete_timer_changed",
      "migrate_to_chat_id",
      "migrate_from_chat_id",
      "pinned_message",
      "invoice",
      "successful_payment",
      "connected_website",
      "passport_data",
      "proximity_alert_triggered",
      "voice_chat_started",
      "voice_chat_ended",
      "voice_chat_participants_invited",
      "reply_markup"
    ]

messageKeyGenerators ::
  Gen (Maybe Integer)
    :+ Gen (Maybe String)
    :+ Gen (Maybe Bool)
    :+ Gen (Maybe User)
    :+ Gen (Maybe Message)
    :+ Gen (Maybe Chat)
    :+ Gen (Maybe [MessageEntity])
    :+ Gen (Maybe Animation)
    :+ Gen (Maybe Audio)
    :+ Gen (Maybe Document)
    :+ Gen (Maybe [PhotoSize])
    :+ Gen (Maybe Sticker)
    :+ Gen (Maybe Video)
    :+ Gen (Maybe VideoNote)
    :+ Gen (Maybe Voice)
    :+ Gen (Maybe Contact)
    :+ Gen (Maybe Dice)
    :+ Gen (Maybe Game)
    :+ Gen (Maybe Poll)
    :+ Gen (Maybe Venue)
    :+ Gen (Maybe Location)
    :+ Gen (Maybe [User])
    :+ Gen (Maybe MessageAutoDeleteTimerChanged)
    :+ Gen (Maybe Invoice)
    :+ Gen (Maybe SuccessfulPayment)
    :+ Gen (Maybe PassportData)
    :+ Gen (Maybe ProximityAlertTriggered)
    :+ Gen (Maybe VoiceChatStarted)
    :+ Gen (Maybe VoiceChatEnded)
    :+ Gen (Maybe VoiceChatParticipantsInvited)
    :+ Gen (Maybe InlineKeyboardMarkup)
messageKeyGenerators =
  genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust

messageWithAllKeys :: IO Message
messageWithAllKeys = generate $ genericArbitraryUG messageKeyGenerators

testMessage :: Spec
testMessage = do
  describe "Test Message JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty Message)
    object <- runIO messageWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just messageKeys
