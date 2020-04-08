{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.Telegram.API.Actions.Data where

import Data.Text (Text)
import Deriving.Aeson
import Web.Telegram.Types
import Web.Telegram.Types.Inline
import Web.Telegram.Types.Stock

data Kick
  = Kick
      { chatId :: ChatId,
        userId :: Integer,
        untilDate :: Maybe Integer
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake Kick

data Unban
  = Unban
      { chatId :: ChatId,
        userId :: Integer
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake Unban

data Restriction
  = Restriction
      { chatId :: ChatId,
        userId :: Integer,
        permissions :: ChatPermissions,
        untilDate :: Maybe Integer
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (ToJSON)
    via Snake Restriction

data Promotion
  = Promotion
      { chatId :: ChatId,
        userId :: Integer,
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
