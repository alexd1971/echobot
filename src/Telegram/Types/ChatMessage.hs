{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.ChatMessage where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier, omitNothingFields),
    ToJSON (toJSON),
    camelTo2,
    defaultOptions,
  )
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (emptyObject)
import Telegram.Types.Animation (Animation)
import Telegram.Types.Audio (Audio)
import Telegram.Types.ChatLocation (ChatLocation)
import Telegram.Types.ChatPermissions (ChatPermissions)
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

data VoiceChatStarted = VoiceChatStarted deriving (Show)

instance FromJSON VoiceChatStarted where
  parseJSON _ = do return VoiceChatStarted

instance ToJSON VoiceChatStarted where
  toJSON _ = emptyObject

newtype VoiceChatEnded = VoiceChatEnded {duration :: Integer} deriving (Show)

$(deriveJSON defaultOptions ''VoiceChatEnded)

newtype VoiceChatParticipantsInvited = VoiceChatParticipantsInvited {users :: Maybe [User]} deriving (Show)

$(deriveJSON defaultOptions ''VoiceChatParticipantsInvited)

data ChatPhoto = ChatPhoto
  { smallFileId :: String,
    smallFileUniqueId :: String,
    bigFileId :: String,
    bigFileUniqueId :: String
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''ChatPhoto
 )

data Chat = Chat
  { chatId :: Integer,
    chatType :: String,
    title :: Maybe String,
    username :: Maybe String,
    firstName :: Maybe String,
    lastName :: Maybe String,
    photo :: Maybe ChatPhoto,
    bio :: Maybe String,
    description :: Maybe String,
    inviteLink :: Maybe String,
    pinnedMessage :: Maybe Message,
    permissions :: Maybe ChatPermissions,
    showModeDelay :: Maybe Integer,
    messageAutoDeleteTime :: Maybe Integer,
    stickerSetName :: Maybe String,
    canSetStickerSet :: Maybe Bool,
    linkedChatId :: Maybe Integer,
    location :: Maybe ChatLocation
  }
  deriving (Show)

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
    forwardDate :: Maybe Integer,
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
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "chatId" -> "id"
           "chatType" -> "type"
           a -> camelTo2 '_' a,
         omitNothingFields = True
       }
     ''Chat
 )

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Message
 )

defaultChat :: Chat
defaultChat =
  Chat
    undefined
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
    Nothing
