{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Web.Telegram.API.Editing
  ( EditMessageText,
    EditMessageCaption,
    EditMessageMedia,
    EditMessageReplyMarkup,
    StopPoll,
    DeleteMessage,
    TextEdit,
    CaptionEdit,
    MediaEdit,
    MarkupEdit,
    PollStop,
  )
where

import Data.Text (Text)
import Servant.API
import Servant.Multipart
import Web.Telegram.API.Common
import Web.Telegram.API.CompoundParam
import Web.Telegram.API.Editing.Data
import Web.Telegram.Types
import Web.Telegram.Types.Input
import Web.Telegram.Types.Update

type Res =
  Get '[JSON] (ReqResult (ReqEither Bool Message))

type EditMessageText =
  Base
    :> "editMessageText"
    :> ReqBody '[JSON] TextEdit
    :> Res

type EditMessageCaption =
  Base
    :> "editMessageCaption"
    :> CaptionEdit
    :> Res

type EditMessageMedia =
  Base
    :> "editMessageMedia"
    :> QueryParam "chat_id" ChatId
    :> QueryParam "message_id" Integer
    :> QueryParam "inline_message_id" Text
    :> CompoundParam Mem "media" InputMedia
    :> Res

type EditMessageReplyMarkup =
  Base
    :> "editMessageReplyMarkup"
    :> ReqBody '[JSON] MarkupEdit
    :> Res

type StopPoll =
  Base
    :> "stopPoll"
    :> ReqBody '[JSON] PollStop
    :> Get '[JSON] (ReqResult Poll)

type DeleteMessage =
  Base
    :> "deleteMessage"
    :> QueryR "chat_id" ChatId
    :> QueryR "message_id" Integer
    :> Get '[JSON] (ReqResult Bool)
