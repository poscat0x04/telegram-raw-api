{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Web.Telegram.API.Editing where

import Data.Aeson
import Data.Text (Text)
import Deriving.Aeson
import Servant.API
import Servant.Multipart
import Web.Telegram.API.Common
import Web.Telegram.API.CompoundParam
import Web.Telegram.Types
import Web.Telegram.Types.Input
import Web.Telegram.Types.Interaction
import Web.Telegram.Types.Stock

type Res =
  Get '[JSON] (ReqResult (ReqEither Bool Message))

type EditMessageText =
  Base
    :> "editMessageText"
    :> ReqBody '[JSON] TextEdit
    :> Res

data TextEdit
  = TextE
      { chatId :: Maybe ChatId,
        messageId :: Maybe Integer,
        inlineMessageId :: Maybe Text,
        text :: Text,
        parseMode :: Maybe ParseMode,
        disalbeWebPagePreview :: Maybe Bool,
        replyMarkup :: Maybe InlineKeyboardMarkup
      }
  deriving (Show, Eq, Generic)
  deriving anyclass (Default)
  deriving
    (FromJSON, ToJSON)
    via Snake TextEdit

type EditMessageCaption =
  Base
    :> "editMessageCaption"
    :> CaptionEdit
    :> Res

data CaptionEdit
  = CaptionE
      { chatId :: Maybe ChatId,
        messageId :: Maybe Integer,
        inlineMessageId :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup
      }
  deriving (Show, Eq, Generic)
  deriving anyclass (Default)
  deriving
    (FromJSON, ToJSON)
    via Snake CaptionEdit

type EditMessageMedia =
  Base
    :> "editMessageMedia"
    :> QueryParam "chat_id" ChatId
    :> QueryParam "message_id" Integer
    :> QueryParam "inline_message_id" Text
    :> CompoundParam Mem "media" InputMedia
    :> Res

data MediaEdit
  = MediaE
      { chatId :: Maybe ChatId,
        messageId :: Maybe Integer,
        inlineMessageId :: Maybe Text,
        media :: InputMedia
      }
  deriving (Show, Eq, Generic)
  deriving anyclass (Default)
  deriving
    (ToJSON)
    via Snake MediaEdit

type EditMessageReplyMarkup =
  Base
    :> "editMessageReplyMarkup"
    :> ReqBody '[JSON] MarkupEdit
    :> Res

data MarkupEdit
  = MarkupEdit
      { chatId :: Maybe ChatId,
        messageId :: Maybe Integer,
        inlineMessageId :: Maybe Text,
        replyMarkup :: Maybe InlineKeyboardMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake MarkupEdit

type StopPoll =
  Base
    :> "stopPoll"
    :> ReqBody '[JSON] PollStop
    :> Get '[JSON] (ReqResult Poll)

data PollStop
  = PollStop
      { chatId :: ChatId,
        messageId :: Integer,
        replyMarkup :: Maybe InlineKeyboardMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake PollStop

type DeleteMessage =
  Base
    :> "deleteMessage"
    :> QueryR "chat_id" ChatId
    :> QueryR "message_id" Integer
    :> Get '[JSON] (ReqResult Bool)
