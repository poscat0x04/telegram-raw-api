{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Telegram.API.Common where

import Data.Text (Text)
import Servant.API

newtype Token = Token Text

instance ToHttpApiData Token where
  toQueryParam (Token t) = "bot" <> t

type Base = Capture "token" Token
