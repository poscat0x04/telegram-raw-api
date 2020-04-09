{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Telegram.API.Common where

import Data.Text (Text)
import GHC.Generics
import Servant.API
import Data.Aeson
import Data.String

newtype Token = Token Text
  deriving (Show, Eq, Generic)
  deriving newtype (IsString, ToJSON, FromJSON)

instance ToHttpApiData Token where
  toQueryParam (Token t) = "bot" <> t

type Base = Capture "token" Token
