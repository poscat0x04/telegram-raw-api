{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.Telegram.API.Editing.Data where

import Data.Text (Text)
import Deriving.Aeson
import Web.Telegram.API.Common
import Web.Telegram.Types
import Web.Telegram.Types.Input
import Web.Telegram.Types.Interaction
import Web.Telegram.Types.Stock

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
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake TextEdit

data CaptionEdit
  = CaptionE
      { chatId :: Maybe ChatId,
        messageId :: Maybe Integer,
        inlineMessageId :: Maybe Text,
        caption :: Maybe Text,
        parseMode :: Maybe ParseMode,
        replyMarkup :: Maybe InlineKeyboardMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake CaptionEdit

data MediaEdit
  = MediaE
      { chatId :: Maybe ChatId,
        messageId :: Maybe Integer,
        inlineMessageId :: Maybe Text,
        media :: InputMedia
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake MediaEdit

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
