{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Telegram.API.Update where

import Data.Text (Text)
import Servant.API
import Servant.Multipart
import Web.Telegram.Types
import Web.Telegram.Types.Input
import Web.Telegram.Types.Update

type GetUpdates =
  "getUpdates"
    :> QueryParam "offset" Integer
    :> QueryParam "limit" Integer
    :> QueryParam "timeout" Integer
    :> QueryParam "allowed_updates" Text
    :> Get '[JSON] (ReqResult [Update])

type SetWebhook =
  "setWebhook"
    :> QueryParam' '[Required, Strict] "url" Text
    :> QueryParam "max_connections" Integer
    :> QueryParam "allowed_updates" [Text]
    :> Get '[JSON] (ReqResult Bool)

type SetWebhook' =
  "setWebhook"
    :> QueryParam' '[Required, Strict] "url" Text
    :> MultipartForm Mem Cert
    :> QueryParam "max_connections" Integer
    :> QueryParam "allowed_updates" [Text]
    :> Get '[JSON] (ReqResult Bool)

type DeleteWebhook =
  "deleteWebhook"
    :> Get '[JSON] (ReqResult Bool)

type GetWebhookInfo =
  "getWebhookInfo"
    :> Get '[JSON] (ReqResult WebhookInfo)
