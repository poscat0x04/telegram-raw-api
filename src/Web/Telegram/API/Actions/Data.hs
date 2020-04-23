{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.Telegram.API.Actions.Data where

import Data.Text (Text)
import Data.Time.Clock.POSIX
import Deriving.Aeson
import Web.Telegram.API.Common
import Web.Telegram.Types
import Web.Telegram.Types.Inline
import Web.Telegram.Types.Stock
import Web.Telegram.Types.Update

data Kick
  = Kick
      { chatId :: ChatId,
        userId :: Int,
        untilDate :: Maybe POSIXTime
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake Kick

data Unban
  = Unban
      { chatId :: ChatId,
        userId :: Int
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake Unban

data Restriction
  = Restriction
      { chatId :: ChatId,
        userId :: Int,
        permissions :: ChatPermissions,
        untilDate :: Maybe POSIXTime
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake Restriction

data Promotion
  = Promotion
      { chatId :: ChatId,
        userId :: Int,
        canChangeInfo :: Maybe Bool,
        canPostMessages :: Maybe Bool,
        canEditMessages :: Maybe Bool,
        canDeleteMessages :: Maybe Bool,
        canInviteUsers :: Maybe Bool,
        canRestrictMembers :: Maybe Bool,
        canPinMessages :: Maybe Bool,
        canPromoteMembers :: Maybe Bool
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake Promotion

data InlineQueryAnswer
  = InlineQueryAnswer
      { inlineQueryId :: Text,
        results :: [InlineQueryResult],
        cacheTime :: Maybe Int,
        isPersonal :: Maybe Bool,
        nextOffset :: Maybe Text,
        switchPmText :: Maybe Text,
        switchPmParameter :: Maybe Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON, FromJSON)
    via Snake InlineQueryAnswer

data CallbackQueryAnswer
  = CQAns
      { callbackQueryId :: Text,
        text :: Maybe Text,
        showAlert :: Maybe Bool,
        url :: Maybe Text,
        cacheTime :: Maybe Int
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON, FromJSON)
    via Snake CallbackQueryAnswer

newtype CommandSet
  = CommandSet
      { commands :: [BotCommand]
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON, FromJSON)
    via Snake CommandSet
