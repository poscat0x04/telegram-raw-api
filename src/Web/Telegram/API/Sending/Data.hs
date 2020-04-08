{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Web.Telegram.API.Sending.Data where

import Data.Text (Text)
import Deriving.Aeson
import Web.Telegram.Types
import Web.Telegram.Types.Interaction
import Web.Telegram.Types.Stock

data SMessage
  = SMsg
      { chatId :: ChatId,
        text :: Text,
        disableWebPagePreview :: Maybe Bool,
        parseMode :: Maybe ParseMode,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake SMessage

data FwdMessage
  = FwdMsg
      { chatId :: ChatId,
        fromChatId :: ChatId,
        messageId :: Integer,
        disableNotification :: Maybe Bool
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake FwdMessage

data PhotoMessage a
  = PhotoMsg
      { chatId :: ChatId,
        photo :: a,
        caption :: Maybe Text,
        disableWebPagePreview :: Maybe Bool,
        parseMode :: Maybe ParseMode,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake (PhotoMessage a)

data AudioMessage a
  = AudioMsg
      { chatId :: ChatId,
        audio :: a,
        caption :: Maybe Text,
        duration :: Maybe Integer,
        performer :: Maybe Text,
        title :: Maybe Text,
        parseMode :: Maybe ParseMode,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake (AudioMessage a)

data DocMessage a
  = DocMsg
      { chatId :: ChatId,
        document :: a,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake (DocMessage a)

data VidMessage a
  = VidMsg
      { chatId :: ChatId,
        video :: a,
        duration :: Maybe Integer,
        width :: Maybe Integer,
        height :: Maybe Integer,
        caption :: Maybe Text,
        supportsStreaming :: Maybe Bool,
        parseMode :: Maybe ParseMode,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake (VidMessage a)

data AnimationMessage a
  = AniMsg
      { chatId :: ChatId,
        animation :: a,
        duration :: Maybe Integer,
        width :: Maybe Integer,
        height :: Maybe Integer,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake (AnimationMessage a)

data VoiceMessage a
  = VoiceMsg
      { chatId :: ChatId,
        voice :: a,
        duration :: Maybe Integer,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake (VoiceMessage a)

data VNMessage a
  = VNMsg
      { chatId :: ChatId,
        video_note :: Text,
        duration :: Maybe Integer,
        length :: Maybe Integer,
        parseMode :: Maybe ParseMode,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving (ToJSON) via Snake (VNMessage a)

data LocationMessage
  = LocationMsg
      { chatId :: ChatId,
        latitude :: Float,
        longitude :: Float,
        livePeriod :: Maybe Integer,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake LocationMessage

data LocationEdit
  = LocationEdit
      { chatId :: Maybe ChatId,
        messageId :: Maybe Integer,
        inlineMessageId :: Maybe Text,
        latitude :: Float,
        longitude :: Float,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake LocationEdit

data LocationStop
  = LocationStop
      { chatId :: Maybe ChatId,
        messageId :: Maybe Integer,
        inlineMessageId :: Maybe Text,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake LocationStop

data VenueMessage
  = VenueMessage
      { chatId :: ChatId,
        latitude :: Float,
        longitude :: Float,
        title :: Text,
        address :: Text,
        foursquareId :: Maybe Text,
        foursquareType :: Maybe Text,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake VenueMessage

data ContactMessage
  = ContactMessage
      { chatId :: ChatId,
        phoneNumber :: Text,
        firstName :: Text,
        lastName :: Maybe Text,
        vcard :: Maybe Text,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake ContactMessage

data PollMessage
  = PollMsg
      { chatid :: ChatId,
        question :: Text,
        options :: [Text],
        isAnonymous :: Bool,
        pollType :: Maybe PollType,
        allowsMultipleAnswers :: Maybe Bool,
        correctOptionId :: Maybe Integer,
        isClosed :: Maybe Bool,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via PrefixedSnake "poll" PollMessage

data DiceMessage
  = DiceMessage
      { chatId :: ChatId,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake DiceMessage

data ChatAction
  = ChatAction
      { chatId :: ChatId,
        action :: Action
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via ChatAction

data StickerMessage
  = StickerMessage
      { chatId :: ChatId,
        sticker :: Text,
        disableNotification :: Maybe Bool,
        replyToMessageId :: Maybe Integer,
        replyMarkup :: Maybe ReplyMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake StickerMessage
