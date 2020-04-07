{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

-- | Sending stuff
module Web.Telegram.API.Sending where

import Data.Text (Text)
import Deriving.Aeson
import Servant.API
import Servant.Multipart
import Web.Telegram.API.CompoundParam
import Web.Telegram.Types
  ( ChatId (..),
    Default (..),
    QueryR,
    ReqEither (..),
    ReqResult (..),
  )
import qualified Web.Telegram.Types as T
import Web.Telegram.Types.Input
import Web.Telegram.Types.Interaction
import Web.Telegram.Types.Stock

type MessageR' =
  QueryParam "disable_notification" Bool
    :> QueryParam "reply_to_message_id" Integer
    :> QueryParam "reply_markup" ReplyMarkup
    :> Get '[JSON] (ReqResult T.Message)

type Res =
  Get '[JSON] (ReqResult T.Message)

type MessageR =
  QueryParam "parse_mode" ParseMode
    :> MessageR'

type SendMessage =
  "sendMessage"
    :> ReqBody '[JSON] Message
    :> Res

data Message
  = Message
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
    via Snake Message

type ForwardMessage =
  "forwardMessage"
    :> ReqBody '[JSON] FwdMessage
    :> Res

data FwdMessage
  = FwdMessage
      { chatId :: ChatId,
        fromChatId :: ChatId,
        messageId :: Integer,
        disableNotification :: Maybe Bool
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake FwdMessage

type SendPhoto =
  "sendPhoto"
    :> ReqBody '[JSON] (PhotoMessage Text)
    :> Res

data PhotoMessage a
  = PM
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

type SendPhoto' photo =
  "sendPhoto"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Photo
    :> QueryParam "caption" Text
    :> MessageR

type SendAudio =
  "sendAudio"
    :> ReqBody '[JSON] (AudioMessage Text)
    :> Res

data AudioMessage a
  = AudioMessage
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

type SendAudio' audio =
  "sendAudio"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Audio
    :> QueryParam "caption" Text
    :> QueryParam "duration" Integer
    :> QueryParam "performer" Text
    :> QueryParam "title" Text
    -- TODO: Handle thumbnail
    :> MessageR

type SendDocument =
  "sendDocument"
    :> ReqBody '[JSON] (DocumentMessage Text)
    :> Res

data DocumentMessage a
  = DocumentMessage
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
    via Snake (DocumentMessage a)

type SendDocument' doc =
  "sendDocument"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Doc
    :> QueryParam "caption" Text
    :> MessageR

type SendVideo =
  "sendVideo"
    :> ReqBody '[JSON] (VideoMessage Text)
    :> Res

data VideoMessage a
  = VideoMessage
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

type SendVideo' =
  "sendVideo"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Video
    :> QueryParam "duration" Integer
    :> QueryParam "width" Integer
    :> QueryParam "height" Integer
    :> QueryParam "caption" Text
    :> QueryParam "supports_streaming" Bool
    :> MessageR

type SendAnimation =
  "sendAnimation"
    :> ReqBody '[JSON] (AnimationMessage Text)
    :> Res

data AnimationMessage a
  = AnimationMessage
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

type SendAnimation' =
  "sendAnimation"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Animation
    :> QueryParam "duration" Integer
    :> QueryParam "width" Integer
    :> QueryParam "height" Integer
    :> QueryParam "caption" Text
    :> MessageR

type SendVoice =
  "sendVoice"
    :> ReqBody '[JSON] (VoiceMessage Text)
    :> Res

data VoiceMessage a
  = VoiceMessage
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

type SendVoice' =
  "sendVoice"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Voice
    :> QueryParam "duration" Integer
    :> QueryParam "caption" Text
    :> MessageR

type SendVideoNote =
  "sendVideoNote"
    :> ReqBody '[JSON] (VideoNoteMessage Text)
    :> Res

data VideoNoteMessage a
  = VideoNoteMessage
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
  deriving (ToJSON) via Snake (VideoNoteMessage a)

type SendVideoNote' =
  "sendVideoNote"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem VideoNote
    :> QueryParam "duration" Integer
    :> QueryParam "length" Integer
    :> MessageR

type SendMediaGroup =
  "sendMediaGroup"
    :> QueryR "chat_id" ChatId
    :> CompoundParams Mem "media" VideoOrPhoto
    :> QueryParam "disable_notification" Bool
    :> QueryParam "reply_to_message_id" Integer
    :> Get '[JSON] (ReqResult [T.Message])

type SendLocation =
  "sendLocation"
    :> ReqBody '[JSON] LocationMessage
    :> Res

data LocationMessage
  = LocationMessage
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

type EditMessageLiveLocation =
  "editMessageLiveLocation"
    :> ReqBody '[JSON] LocationEdit
    :> Get '[JSON] (ReqResult (ReqEither T.Message Bool))

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

type StopMessageLiveLocation =
  "stopMessageLiveLocation"
    :> ReqBody '[JSON] LocationStop
    :> Get '[JSON] (ReqResult (ReqEither T.Message Bool))

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

type SendVenue =
  "sendVenue"
    :> ReqBody '[JSON] VenueMessage
    :> Res

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

type SendContact =
  "sendContact"
    :> ReqBody '[JSON] ContactMessage
    :> Res

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

type SendPoll =
  "sendPoll"
    :> ReqBody '[JSON] PollMessage
    :> Res

data PollMessage
  = PollMessage
      { chatid :: ChatId,
        question :: Text,
        options :: [Text],
        isAnonymous :: Bool,
        pollType :: Maybe T.PollType,
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

type SendDice =
  "sendDice"
    :> ReqBody '[JSON] DiceMessage
    :> Res

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

type SendChatAction =
  "sendChatAction"
    :> ReqBody '[JSON] ChatAction
    :> Get '[JSON] (ReqResult Bool)

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

type SendSticker =
  "sendSticker"
    :> ReqBody '[JSON] StickerMessage
    :> Res

type SendSticker' sticker =
  "sendSticker"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Sticker
    :> MessageR'
