{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Telegram.API.Editing where

import Data.Text (Text)
import Servant.API
import Servant.Multipart
import Web.Telegram.API.CompoundParam
import Web.Telegram.Types
import Web.Telegram.Types.Input
import Web.Telegram.Types.Interaction

type Tail =
  QueryParam "reply_markup" InlineKeyboardMarkup
    :> Get '[JSON] (ReqResult (ReqEither Bool Message))

type EditMessageText =
  "editMessageText"
    :> QueryParam "chat_id" ChatId
    :> QueryParam "message_id" Integer
    :> QueryParam "inline_message_id" Text
    :> QueryR "text" Text
    :> QueryParam "parse_mode" ParseMode
    :> QueryParam "disable_web_page_preview" Bool
    :> Tail

type EditMessageCaption =
  "editMessageCaption"
    :> QueryParam "chat_id" ChatId
    :> QueryParam "message_id" Integer
    :> QueryParam "inline_message_id" Text
    :> QueryParam "caption" Text
    :> QueryParam "parse_mode" ParseMode
    :> Tail

type EditMessageMedia =
  "editMessageMedia"
    :> QueryParam "chat_id" ChatId
    :> QueryParam "message_id" Integer
    :> QueryParam "inline_message_id" Text
    :> CompoundParam Mem "media" InputMedia
    :> Tail

type EditMessageReplyMarkup =
  "editMessageReplyMarkup"
    :> QueryParam "chat_id" ChatId
    :> QueryParam "message_id" Integer
    :> QueryParam "inline_message_id" Text
    :> Tail

type StopPoll =
  "stopPoll"
    :> QueryR "chat_id" ChatId
    :> QueryR "message_id" Integer
    :> QueryParam "reply_markup" InlineKeyboardMarkup
    :> Get '[JSON] (ReqResult Poll)

type DeleteMessage =
  "deleteMessage"
    :> QueryR "chat_id" ChatId
    :> QueryR "message_id" Integer
    :> Get '[JSON] (ReqResult Bool)
