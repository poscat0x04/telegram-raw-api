{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Telegram.API.Getting where

import Data.Text (Text)
import Servant.API
import Web.Telegram.Types

type GetMe =
  "getMe"
    :> Get '[JSON] (ReqResult User)

type GetUserProfilePhotos =
  "getUserProfilePhotos"
    :> QueryR "user_id" Integer
    :> QueryParam "offset" Integer
    :> QueryParam "limit" Integer
    :> Get '[JSON] (ReqResult UserProfilePhotos)

type GetFile =
  "getFile"
    :> QueryR "file_id" Text
    :> Get '[JSON] (ReqResult File)

type GetChat =
  "getChat"
    :> QueryR "chat_id" ChatId
    :> Get '[JSON] (ReqResult Chat)

type GetChatAdministrators =
  "getChatAdministrators"
    :> QueryR "chat_id" ChatId
    :> Get '[JSON] (ReqResult [ChatMember])

type GetChatMembersCount =
  "getChatMembersCount"
    :> QueryR "chat_id" ChatId
    :> Get '[JSON] (ReqResult Integer)

type GetChatMember =
  "getChatMember"
    :> QueryR "chat_id" ChatId
    :> QueryR "user_id" Integer
    :> Get '[JSON] (ReqResult ChatMember)

type GetMyCommands =
  "getMyCommands"
    :> Get '[JSON] (ReqResult [BotCommand])

type GetStickerSet =
  "getStickerSet"
    :> QueryR "name" Text
    :> Get '[JSON] (ReqResult StickerSet)
