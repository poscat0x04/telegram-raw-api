{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Telegram.API.Common where

import Data.Text (Text)
import GHC.Generics
import Servant.API

newtype Token = Token Text
  deriving (Show, Eq, Generic)

instance ToHttpApiData Token where
  toQueryParam (Token t) = "bot" <> t

type Base = Capture "token" Token
