{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Web.Telegram.API.Update where

import Data.Text (Text)
import Deriving.Aeson
import Servant.API
import Web.Telegram.Types
import Web.Telegram.Types.Stock
import Web.Telegram.Types.Update

data Polling
  = Polling
      { offset :: Maybe Integer,
        limit :: Maybe Integer,
        timeout :: Maybe Integer,
        allowedUpdates :: Maybe Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake Polling

type GetUpdates =
  "getUpdates"
    :> ReqBody '[JSON] Polling
    :> Get '[JSON] (ReqResult [Update])

data WebhookSetting
  = WebhookSetting
      { url :: Text,
        maxConnections :: Maybe Integer,
        allowedUpdates :: Maybe [Text]
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake WebhookSetting

type SetWebhook =
  "setWebhook"
    :> ReqBody '[JSON] WebhookSetting
    :> Get '[JSON] (ReqResult Bool)

type DeleteWebhook =
  "deleteWebhook"
    :> Get '[JSON] (ReqResult Bool)

type GetWebhookInfo =
  "getWebhookInfo"
    :> Get '[JSON] (ReqResult WebhookInfo)
