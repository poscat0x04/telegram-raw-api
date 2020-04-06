{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

-- | Actions that the bot can perform
module Web.Telegram.API.Actions where

import Data.Default
import Data.Text (Text)
import Deriving.Aeson
import GHC.Generics
import Servant.API
import Servant.Multipart
import Web.Telegram.API.CompoundParam
import Web.Telegram.API.Utils
import Web.Telegram.Types
import Web.Telegram.Types.Inline
import Web.Telegram.Types.Input

type KickChatMember =
  "kickChatmember"
    :> QueryR "chat_id" ChatId
    :> QueryR "user_id" Integer
    :> QueryParam "until_date" Integer
    :> Get '[JSON] (ReqResult Bool)

type UnbanChatMember =
  "unbanChatMember"
    :> QueryR "chat_id" ChatId
    :> QueryR "user_id" Integer
    :> Get '[JSON] (ReqResult Bool)

type RestrictChatMember =
  "restrictChatMember"
    :> QueryR "chat_id" ChatId
    :> QueryR "user_id" Integer
    :> QueryR "permissions" ChatPermissions
    :> QueryParam "until_date" Integer
    :> Get '[JSON] (ReqResult Bool)

type PromoteChatMember =
  "promoteChatMember"
    :> QueryR "chat_id" ChatId
    :> QueryR "user_id" Integer
    :> QueryParam "can_change_info" Bool
    :> QueryParam "can_post_messages" Bool
    :> QueryParam "can_edit_messages" Bool
    :> QueryParam "can_delete_messages" Bool
    :> QueryParam "can_invite_users" Bool
    :> QueryParam "can_restrict_members" Bool
    :> QueryParam "can_pin_messages" Bool
    :> QueryParam "can_promote_members" Bool
    :> Get '[JSON] (ReqResult Bool)

type SetChatAdministratorCustomTitle =
  "setChatAdministratorCustomTitle"
    :> QueryR "chat_id" ChatId
    :> QueryR "user_id" Integer
    :> QueryParam "custom_title" Text
    :> Get '[JSON] (ReqResult Bool)

type SetChatPermissions =
  "setChatPermissions"
    :> QueryR "chat_id" ChatId
    :> QueryR "permissions" ChatPermissions
    :> Get '[JSON] (ReqResult Bool)

type ExportChatInviteLink =
  "exportChatInviteLink"
    :> QueryR "chat_id" ChatId
    :> Get '[JSON] (ReqResult Text)

type SetChatPhoto =
  "setChatPhoto"
    :> QueryR "chat_id" ChatId
    :> CompoundParam Mem "photo" InputFile
    :> Get '[JSON] (ReqResult Bool)

type DeleteChatPhoto =
  "deleteChatPhoto"
    :> QueryR "chat_id" ChatId
    :> Get '[JSON] (ReqResult Bool)

type SetChatTitle =
  "setChatTitle"
    :> QueryR "chat_id" ChatId
    :> QueryR "title" Text
    :> Get '[JSON] (ReqResult Bool)

type SetChatDescription =
  "setChatDescription"
    :> QueryR "chat_id" ChatId
    :> QueryParam "description" Text
    :> Get '[JSON] (ReqResult Bool)

type PinChatMessage =
  "pinChatMessage"
    :> QueryR "chat_id" ChatId
    :> QueryR "message_id" Integer
    :> QueryParam "disable_notification" Bool
    :> Get '[JSON] (ReqResult Bool)

type UnpinChatMessage =
  "unpinChatMessage"
    :> QueryR "chat_id" ChatId
    :> Get '[JSON] (ReqResult Bool)

type LeaveChat =
  "leaveChat"
    :> QueryR "chat_id" ChatId
    :> Get '[JSON] (ReqResult Bool)

type SetChatStickerSet =
  "setChatStickerSet"
    :> QueryR "chat_id" ChatId
    :> QueryR "sticker_set_name" Text
    :> Get '[JSON] (ReqResult Bool)

type DeleteChatStickerSet =
  "deleteChatStickerSet"
    :> QueryR "chat_id" ChatId
    :> Get '[JSON] (ReqResult Bool)

type UploadStickerFile' png =
  "uploadStickerFile"
    :> QueryR "user_id" Integer
    :> png
    :> Get '[JSON] (ReqResult File)

type UploadStickerFileU =
  UploadStickerFile' (MultipartForm Mem PngSticker)

type UploadStickerFile =
  UploadStickerFile' (QueryR "png_sticker" Text)

type CreateNewStickerSet' png =
  "createNewStickerSet"
    :> QueryR "user_id" Integer
    :> QueryR "name" Text
    :> QueryR "title" Text
    :> png
    :> MultipartForm Mem TgsSticker
    :> QueryR "emojis" Text
    :> QueryParam "contains_masks" Bool
    :> QueryParam "mask_position" MaskPosition
    :> Get '[JSON] (ReqResult Bool)

type CreateNewStickerSetU =
  CreateNewStickerSet' (MultipartForm Mem PngSticker)

type CreateNewStickerSet =
  CreateNewStickerSet' (QueryR "png_sticker" Text)

type AddStickerToSet' png =
  "addStickerToSet"
    :> QueryR "user_id" Integer
    :> QueryR "name" Text
    :> png
    :> MultipartForm Mem TgsSticker
    :> QueryR "emojis" Text
    :> QueryR "mask_position" MaskPosition
    :> Get '[JSON] (ReqResult Bool)

type AddStickerToSetU =
  AddStickerToSet' (MultipartForm Mem PngSticker)

type AddStickerToSet =
  AddStickerToSet' (QueryR "png_sticker" Text)

type SetStickerPositionInSet =
  "setStickerPositionInSet"
    :> QueryR "sticker" Text
    :> QueryR "positon" Integer
    :> Get '[JSON] (ReqResult Bool)

type DeleteStickerFromSet =
  "deleteStickerFromSet"
    :> QueryR "sticker" Text
    :> Get '[JSON] (ReqResult Bool)

type SetStickerSetThumb' t =
  "setStickerSetThumb"
    :> QueryR "name" Text
    :> QueryR "user_id" Integer
    :> t
    :> Get '[JSON] (ReqResult Bool)

type SetStickerSetThumbU =
  SetStickerSetThumb' (MultipartForm Mem PngSticker)

type SetStickerSetThumb =
  SetStickerSetThumb' (QueryR "thumb" Text)

type AnswerInlineQuery =
  "answerInlineQuery"
    :> QueryR "inline_query_id" Text
    :> QueryR "results" [InlineQueryResult]
    :> QueryParam "cache_time" Integer
    :> QueryParam "is_personal" Bool
    :> QueryParam "next_offset" Text
    :> QueryParam "switch_pm_text" Text
    :> QueryParam "switch_pm_parameter" Text
    :> Get '[JSON] (ReqResult Bool)

data InlineQueryAnswer
  = InlineQueryAnswer
      { inlineQueryId :: Text,
        results :: [InlineQueryResult],
        cacheTime :: Maybe Integer,
        isPersonal :: Maybe Bool,
        nextOffset :: Maybe Text,
        switchPmText :: Maybe Text,
        switchPmParameter :: Maybe Text
      }
  deriving (Show, Eq, Generic)
  deriving anyclass (Default)
  deriving
    (ToJSON, FromJSON)
    via Snake InlineQueryAnswer
