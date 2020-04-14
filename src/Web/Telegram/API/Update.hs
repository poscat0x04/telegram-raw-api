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
import Web.Telegram.API.Common
import Web.Telegram.Types
import Web.Telegram.Types.Stock
import Web.Telegram.Types.Update
import Web.Telegram.Types.UpdateType

data Polling
  = Polling
      { offset :: Maybe Integer,
        limit :: Maybe Integer,
        timeout :: Maybe Integer,
        allowedUpdates :: Maybe Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON, FromJSON)
    via Snake Polling

data WebhookSetting
  = WebhookSetting
      { url :: Text,
        maxConnections :: Maybe Integer,
        allowedUpdates :: Maybe [UpdateType]
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON, FromJSON)
    via Snake WebhookSetting

type GetUpdates =
  Base
    :> "getUpdates"
    :> ReqBody '[JSON] Polling
    :> Get '[JSON] (ReqResult [Update])

type SetWebhook =
  Base
    :> "setWebhook"
    :> ReqBody '[JSON] WebhookSetting
    :> Get '[JSON] (ReqResult Bool)

type DeleteWebhook =
  Base
    :> "deleteWebhook"
    :> Get '[JSON] (ReqResult Bool)

type GetWebhookInfo =
  Base
    :> "getWebhookInfo"
    :> Get '[JSON] (ReqResult WebhookInfo)
