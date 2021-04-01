{-# LANGUAGE OverloadedStrings #-}

module Telegram.Types.Message where

import Telegram.Types.Animation (Animation)
import Telegram.Types.Audio (Audio)
import {-# SOURCE #-} Telegram.Types.Chat (Chat)
import Telegram.Types.Contact (Contact)
import Telegram.Types.Dice (Dice)
import Telegram.Types.Document (Document)
import Telegram.Types.Game (Game)
import Telegram.Types.InlineKeyboardMarkup (InlineKeyboardMarkup)
import Telegram.Types.Invoice (Invoice)
import Telegram.Types.Location (Location)
import Telegram.Types.MessageAutoDeleteTimerChanged (MessageAutoDeleteTimerChanged)
import Telegram.Types.MessageEntity (MessageEntity)
import Telegram.Types.PassportData (PassportData)
import Telegram.Types.PassportFile (PassportFile)
import Telegram.Types.PhotoSize (PhotoSize)
import Telegram.Types.Poll (Poll)
import Telegram.Types.ProximityAlertTriggered (ProximityAlertTriggered)
import Telegram.Types.Sticker (Sticker)
import Telegram.Types.SuccessfulPayment (SuccessfulPayment)
import Telegram.Types.User (User)
import Telegram.Types.Venue (Venue)
import Telegram.Types.Video (Video)
import Telegram.Types.VideoNote (VideoNote)
import Telegram.Types.Voice (Voice)

data Message = Message
  { messageId :: Integer,
    from :: Maybe User,
    senderChat :: Maybe Chat,
    date :: Integer,
    chat :: Chat,
    forwardFrom :: Maybe User,
    forwardFromChat :: Maybe Chat,
    forwardFromMessageId :: Maybe Integer,
    forwardSignature :: Maybe String,
    forwardSenderName :: Maybe String,
    forwardDate :: Integer,
    replyToMessage :: Maybe Message,
    viaBot :: Maybe User,
    editDate :: Maybe Integer,
    mediaGroupId :: Maybe String,
    authorSignature :: Maybe String,
    text :: Maybe String,
    entities :: Maybe [MessageEntity],
    animation :: Maybe Animation,
    audio :: Maybe Audio,
    document :: Maybe Document,
    photo :: Maybe [PhotoSize],
    sticker :: Maybe Sticker,
    video :: Maybe Video,
    videoNote :: Maybe VideoNote,
    voice :: Maybe Voice,
    caption :: Maybe String,
    captionEntities :: Maybe [MessageEntity],
    contact :: Maybe Contact,
    dice :: Maybe Dice,
    game :: Maybe Game,
    poll :: Maybe Poll,
    venue :: Maybe Venue,
    location :: Maybe Location,
    newChatMembers :: Maybe [User],
    leftChatMember :: Maybe User,
    newChatTitle :: Maybe String,
    newChatPhoto :: Maybe [PhotoSize],
    deleteChatPhoto :: Maybe Bool,
    groupChatCreated :: Maybe Bool,
    supergroupChatCreated :: Maybe Bool,
    channelChatCreated :: Maybe Bool,
    messageAutoDeleteTimerChanged :: Maybe MessageAutoDeleteTimerChanged,
    migrateToChatId :: Maybe Integer,
    migrateFromChatId :: Maybe Integer,
    pinnedMessage :: Maybe Message,
    invoice :: Maybe Invoice,
    successfulPayment :: Maybe SuccessfulPayment,
    connectedWebsite :: Maybe String,
    passportData :: Maybe PassportData,
    proximityAlertTriggerd :: Maybe ProximityAlertTriggered,
    voisChatStarted :: Maybe VoiceChatStarted,
    voiceChatEnded :: Maybe VoiceChatEnded,
    voiceChatParticipantsInvited :: Maybe VoiceChatParticipantsInvited,
    replyMarkup :: Maybe InlineKeyboardMarkup
  }

data VoiceChatStarted

newtype VoiceChatEnded = VoiceChatEnded {duration :: Integer}

newtype VoiceChatParticipantsInvited = VoiceChatParticipantsInvited {users :: Maybe [User]}

defaultMessage :: Message
defaultMessage =
  Message
    undefined
    Nothing
    Nothing
    undefined
    undefined
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    undefined
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
