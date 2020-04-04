{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Sending stuff
module Web.Telegram.API.Sending where

import Data.Text (Text)
import Servant.API
import Servant.Multipart
import Web.Telegram.API.CompoundParam
import Web.Telegram.Types
  ( ChatId (..),
    Message (..),
    QueryR,
    ReqEither (..),
    ReqResult (..),
  )
import Web.Telegram.Types.Input
import Web.Telegram.Types.Interaction

type MessageR' =
  QueryParam "disable_notification" Bool
    :> QueryParam "reply_to_message_id" Integer
    :> QueryParam "reply_markup" ReplyMarkup
    :> Get '[JSON] (ReqResult Message)

type MessageR =
  QueryParam "parse_mode" ParseMode
    :> MessageR'

type SendMessage =
  "sendMessage"
    :> QueryR "chat_id" ChatId
    :> QueryR "text" Text
    :> QueryParam "disable_web_page_preview" Bool
    :> MessageR

type ForwardMessage =
  "forwardMessage"
    :> QueryR "chat_id" ChatId
    :> QueryR "from_chat_id" ChatId
    :> QueryParam "disable_notification" Bool
    :> QueryR "message_id" Integer
    :> Get '[JSON] (ReqResult Message)

type SendPhoto' photo =
  "sendPhoto"
    :> QueryR "chat_id" ChatId
    :> photo
    :> QueryParam "caption" Text
    :> MessageR

type SendPhotoU =
  SendPhoto' (MultipartForm Mem Photo)

type SendPhoto =
  SendPhoto' (QueryR "photo" Text)

type SendAudio' audio =
  "sendAudio"
    :> QueryR "chat_id" ChatId
    :> audio
    :> QueryParam "caption" Text
    :> QueryParam "duration" Integer
    :> QueryParam "performer" Text
    :> QueryParam "title" Text
    -- TODO: Handle thumbnail
    :> MessageR

type SendAudioU =
  SendAudio' (MultipartForm Mem Audio)

type SendAudio =
  SendAudio' (QueryR "audio" Text)

type SendDocument' doc =
  "sendDocument"
    :> QueryR "chat_id" ChatId
    :> doc
    :> QueryParam "caption" Text
    :> MessageR

type SendDocumentU =
  SendDocument' (MultipartForm Mem Doc)

type SendDocument =
  SendDocument' (QueryR "document" Text)

type SendVideo' vid =
  "sendVideo"
    :> QueryR "chat_id" ChatId
    :> vid
    :> QueryParam "duration" Integer
    :> QueryParam "width" Integer
    :> QueryParam "height" Integer
    :> QueryParam "caption" Text
    :> QueryParam "supports_streaming" Bool
    :> MessageR

type SendVideoU =
  SendVideo' (MultipartForm Mem Video)

type SendVideo =
  SendVideo' (QueryR "video" Text)

type SendAnimation' animation =
  "sendAnimation"
    :> QueryR "chat_id" ChatId
    :> animation
    :> QueryParam "duration" Integer
    :> QueryParam "width" Integer
    :> QueryParam "height" Integer
    :> QueryParam "caption" Text
    :> MessageR

type SendAnimationU =
  SendAnimation' (MultipartForm Mem Animation)

type SendAnimation =
  SendAnimation' (QueryR "animation" Text)

type SendVoice' voice =
  "sendVoice"
    :> QueryR "chat_id" ChatId
    :> voice
    :> QueryParam "duration" Integer
    :> QueryParam "caption" Text
    :> MessageR

type SendVideoNote' vn =
  "sendVideoNote"
    :> QueryR "chat_id" ChatId
    :> vn
    :> QueryParam "duration" Integer
    :> QueryParam "length" Integer
    :> MessageR

type SendVideoNoteU =
  SendVideoNote' (MultipartForm Mem VideoNote)

type SendVideoNote =
  SendVideoNote' (QueryR "video_note" Text)

type SendMediaGroup =
  "sendMediaGroup"
    :> QueryR "chat_id" ChatId
    :> CompoundParams Mem "media" VideoOrPhoto
    :> QueryParam "disable_notification" Bool
    :> QueryParam "reply_to_message_id" Integer
    :> Get '[JSON] (ReqResult [Message])

type SendLocation =
  "sendLocation"
    :> QueryR "chat_id" ChatId
    :> QueryR "latitude" Float
    :> QueryR "longitude" Float
    :> QueryParam "live_period" Integer
    :> MessageR'

type EditMessageLiveLocation =
  "editMessageLiveLocation"
    :> QueryParam "chat_id" ChatId
    :> QueryParam "message_id" Integer
    :> QueryParam "inline_message_id" Text
    :> QueryR "latitude" Float
    :> QueryR "longitude" Float
    :> QueryParam "reply_markup" ReplyMarkup
    :> Get '[JSON] (ReqResult (ReqEither Message Bool))

type StopMessageLiveLocation =
  "stopMessageLiveLocation"
    :> QueryParam "chat_id" ChatId
    :> QueryParam "message_id" Integer
    :> QueryParam "inline_message_id" Text
    :> QueryParam "reply_markup" ReplyMarkup
    :> Get '[JSON] (ReqResult (ReqEither Message Bool))

type SendVenue =
  "sendVenue"
    :> QueryR "chat_id" ChatId
    :> QueryR "latitude" Float
    :> QueryR "longitude" Float
    :> QueryR "title" Text
    :> QueryR "address" Text
    :> QueryParam "foursquare_id" Text
    :> QueryParam "foursquare_type" Text
    :> MessageR'

type SendContact =
  "sendContact"
    :> QueryR "chat_id" ChatId
    :> QueryR "phone_number" Text
    :> QueryR "first_name" Text
    :> QueryParam "last_name" Text
    :> QueryParam "vcard" Text
    :> MessageR'

type SendPoll =
  "sendPoll"
    :> QueryR "chat_id" ChatId
    :> QueryR "question" Text
    :> QueryR "options" [Text]
    :> QueryParam "is_anonymous" Bool
    :> QueryParam "type" Text
    :> QueryParam "allows_multiple_answers" Bool
    :> QueryParam "correct_option_id" Integer
    :> QueryParam "is_closed" Bool
    :> MessageR'

type SendDice =
  "sendDice"
    :> QueryR "chat_id" ChatId
    :> MessageR'

type SendChatAction =
  "sendChatAction"
    :> QueryR "chat_id" ChatId
    :> QueryR "action" Text
    :> Get '[JSON] (ReqResult Bool)

type SendSticker' sticker =
  "sendSticker"
    :> QueryR "chat_id" ChatId
    :> sticker
    :> MessageR'

type SendStickerU =
  SendSticker' (MultipartForm Mem Sticker)

type SendSticker =
  SendSticker' (QueryR "sticker" Text)
