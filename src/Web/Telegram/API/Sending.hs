{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

-- | Sending stuff
module Web.Telegram.API.Sending
  ( SendMessage,
    ForwardMessage,
    SendPhoto,
    SendPhoto',
    SendAudio,
    SendAudio',
    SendDocument,
    SendDocument',
    SendVideo,
    SendVideo',
    SendAnimation,
    SendAnimation',
    SendVoice,
    SendVoice',
    SendVideoNote,
    SendVideoNote',
    SendMediaGroup,
    SendLocation,
    EditMessageLiveLocation,
    StopMessageLiveLocation,
    SendVenue,
    SendContact,
    SendPoll,
    SendDice,
    SendChatAction,
    SendSticker,
    SendSticker',
    SMessage,
    FwdMessage,
    PhotoMessage,
    AudioMessage,
    DocMessage,
    VidMessage,
    AnimationMessage,
    VoiceMessage,
    VNMessage,
    LocationEdit,
    LocationStop,
    VenueMessage,
    ContactMessage,
    PollMessage,
    DiceMessage,
    ChatAction,
    StickerMessage,
  )
where

import Data.Text (Text)
import Servant.API
import Servant.Multipart
import Web.Telegram.API.Common
import Web.Telegram.API.CompoundParam
import Web.Telegram.API.Sending.Data
import Web.Telegram.Types
  ( ParseMode (..),
    QueryR,
  )
import qualified Web.Telegram.Types as T
import Web.Telegram.Types.Input
import Web.Telegram.Types.Interaction
import Web.Telegram.Types.Update

type MessageR' =
  QueryParam "disable_notification" Bool
    :> QueryParam "reply_to_message_id" Int
    :> QueryParam "reply_markup" ReplyMarkup
    :> Get '[JSON] (ReqResult T.Message)

type Res =
  Get '[JSON] (ReqResult T.Message)

type MessageR =
  QueryParam "parse_mode" ParseMode
    :> MessageR'

type SendMessage =
  Base
    :> "sendMessage"
    :> ReqBody '[JSON] SMessage
    :> Res

type ForwardMessage =
  Base
    :> "forwardMessage"
    :> ReqBody '[JSON] FwdMessage
    :> Res

type SendPhoto =
  Base
    :> "sendPhoto"
    :> ReqBody '[JSON] (PhotoMessage Text)
    :> Res

type SendPhoto' photo =
  Base
    :> "sendPhoto"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Photo
    :> QueryParam "caption" Text
    :> MessageR

type SendAudio =
  Base
    :> "sendAudio"
    :> ReqBody '[JSON] (AudioMessage Text)
    :> Res

type SendAudio' audio =
  Base
    :> "sendAudio"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Audio
    :> QueryParam "caption" Text
    :> QueryParam "duration" Int
    :> QueryParam "performer" Text
    :> QueryParam "title" Text
    -- TODO: Handle thumbnail
    :> MessageR

type SendDocument =
  Base
    :> "sendDocument"
    :> ReqBody '[JSON] (DocMessage Text)
    :> Res

type SendDocument' doc =
  Base
    :> "sendDocument"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Doc
    :> QueryParam "caption" Text
    :> MessageR

type SendVideo =
  Base
    :> "sendVideo"
    :> ReqBody '[JSON] (VidMessage Text)
    :> Res

type SendVideo' =
  Base
    :> "sendVideo"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Video
    :> QueryParam "duration" Int
    :> QueryParam "width" Int
    :> QueryParam "height" Int
    :> QueryParam "caption" Text
    :> QueryParam "supports_streaming" Bool
    :> MessageR

type SendAnimation =
  Base
    :> "sendAnimation"
    :> ReqBody '[JSON] (AnimationMessage Text)
    :> Res

type SendAnimation' =
  Base
    :> "sendAnimation"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Animation
    :> QueryParam "duration" Int
    :> QueryParam "width" Int
    :> QueryParam "height" Int
    :> QueryParam "caption" Text
    :> MessageR

type SendVoice =
  Base
    :> "sendVoice"
    :> ReqBody '[JSON] (VoiceMessage Text)
    :> Res

type SendVoice' =
  Base
    :> "sendVoice"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Voice
    :> QueryParam "duration" Int
    :> QueryParam "caption" Text
    :> MessageR

type SendVideoNote =
  Base
    :> "sendVideoNote"
    :> ReqBody '[JSON] (VNMessage Text)
    :> Res

type SendVideoNote' =
  Base
    :> "sendVideoNote"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem VideoNote
    :> QueryParam "duration" Int
    :> QueryParam "length" Int
    :> MessageR

type SendMediaGroup =
  Base
    :> "sendMediaGroup"
    :> QueryR "chat_id" ChatId
    :> CompoundParams Mem "media" VideoOrPhoto
    :> QueryParam "disable_notification" Bool
    :> QueryParam "reply_to_message_id" Int
    :> Get '[JSON] (ReqResult [T.Message])

type SendLocation =
  Base
    :> "sendLocation"
    :> ReqBody '[JSON] LocationMessage
    :> Res

type EditMessageLiveLocation =
  Base
    :> "editMessageLiveLocation"
    :> ReqBody '[JSON] LocationEdit
    :> Get '[JSON] (ReqResult (ReqEither T.Message Bool))

type StopMessageLiveLocation =
  Base
    :> "stopMessageLiveLocation"
    :> ReqBody '[JSON] LocationStop
    :> Get '[JSON] (ReqResult (ReqEither T.Message Bool))

type SendVenue =
  Base
    :> "sendVenue"
    :> ReqBody '[JSON] VenueMessage
    :> Res

type SendContact =
  Base
    :> "sendContact"
    :> ReqBody '[JSON] ContactMessage
    :> Res

type SendPoll =
  Base
    :> "sendPoll"
    :> ReqBody '[JSON] PollMessage
    :> Res

type SendDice =
  Base
    :> "sendDice"
    :> ReqBody '[JSON] DiceMessage
    :> Res

type SendChatAction =
  Base
    :> "sendChatAction"
    :> ReqBody '[JSON] ChatAction
    :> Get '[JSON] (ReqResult Bool)

type SendSticker =
  Base
    :> "sendSticker"
    :> ReqBody '[JSON] StickerMessage
    :> Res

type SendSticker' sticker =
  Base
    :> "sendSticker"
    :> QueryR "chat_id" ChatId
    :> MultipartForm Mem Sticker
    :> MessageR'
