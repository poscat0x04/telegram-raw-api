{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Telegram.API.Common where

import Data.Aeson
import Data.Hashable
import Data.Int
import Data.String
import Data.Text (Text)
import Deriving.Aeson
import Servant.API
import Web.Telegram.Types
import Web.Telegram.Types.Stock

data ChatId
  = ChatId Int64
  | ChanId Text
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (FromJSON, ToJSON) via UntaggedSum ChatId

instance ToHttpApiData ChatId where
  toQueryParam (ChatId i) = toQueryParam i
  toQueryParam (ChanId t) = t

newtype Token = Token Text
  deriving (Show, Eq, Generic)
  deriving newtype (IsString, ToJSON, FromJSON)

instance Default Token where
  def = "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11"

instance ToHttpApiData Token where
  toQueryParam (Token t) = "bot" <> t

type Base = Capture "token" Token
