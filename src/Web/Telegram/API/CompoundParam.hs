{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Compound parameters
module Web.Telegram.API.CompoundParam
  ( CompoundParam,
    CompoundParams,
    Warp (..),
  )
where

import qualified Data.ByteString.Lazy as LBS
import Data.Kind
import Data.Proxy
import qualified Data.Text as T
import GHC.TypeLits
import Servant.API
import Servant.Client
import Servant.Multipart
import Web.Telegram.Types

data CompoundParam (tag :: Type) (sym :: Symbol) (a :: Type)

data CompoundParams (tag :: Type) (sym :: Symbol) (a :: Type)

instance
  (KnownSymbol sym, ToHttpApiData a, HasClient m api, ToMultipart tag a, MultipartBackend tag) =>
  HasClient m (CompoundParam tag sym a :> api)
  where
  type
    Client m (CompoundParam tag sym a :> api) =
      (LBS.ByteString, a) -> Client m api
  clientWithRoute pm _ req (boundary, param) =
    clientWithRoute pm (Proxy @(MultipartForm tag a :> QueryR sym a :> api)) req (boundary, param) param
  hoistClientMonad pm _ f cl arg =
    hoistClientMonad pm (Proxy @api) f (cl arg)

instance
  (KnownSymbol sym, ToHttpApiData a, HasClient m api, ToMultipart tag a, MultipartBackend tag) =>
  HasClient m (CompoundParams tag sym a :> api)
  where
  type
    Client m (CompoundParams tag sym a :> api) =
      (LBS.ByteString, [a]) -> Client m api
  clientWithRoute pm _ req (boundary, param) =
    clientWithRoute pm (Proxy @(MultipartForm tag (Fold [a]) :> QueryR sym (Warp [a]) :> api)) req (boundary, coe param) (coe param)
  hoistClientMonad pm _ f cl arg =
    hoistClientMonad pm (Proxy @api) f (cl arg)

newtype Warp a = Warp a

instance (ToHttpApiData a) => ToHttpApiData (Warp [a]) where
  toQueryParam (Warp l) =
    "["
      <> T.intercalate "," (fmap toQueryParam l)
      <> "]"

newtype Fold a = Fold {unfold :: a}

instance Semigroup (Fold (MultipartData tag)) where
  Fold d1 <> Fold d2 = Fold $ MultipartData (inputs d1 <> inputs d2) (files d1 <> files d2)

instance Monoid (Fold (MultipartData tag)) where
  mempty = Fold $ MultipartData mempty mempty

instance (ToMultipart tag a) => ToMultipart tag (Fold [a]) where
  toMultipart (Fold l) = unfold $ foldMap (Fold . toMultipart) l
