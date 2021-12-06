{-|
Module     : Control.Eff.Except
Maintainer : Mohammad Hasani (the-dr-lazy.github.io) <the-dr-lazy@pm.me>
Copyright  : (c) 2021 Effecful
License    : MPL 2.0

This Source Code Form is subject to the terms of the Mozilla Public
License, version 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Control.Eff.Except where

import Data.Either
import Data.Maybe
import Prelude
import Type.Proxy

import Control.Eff (Eff)
import Control.Eff as Eff
import Control.Monad.Error.Class (catchError, throwError)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Exception (Error)
import Type.Row (type (+))
import Type.Row as Row
import Unsafe.Coerce (unsafeCoerce)

data ExceptF :: Type -> Eff.Algebra
data ExceptF e m a = Except String a

type Except :: Type -> Row Eff.Algebra -> Row Eff.Algebra
type Except e r = (except :: ExceptF e | r)

tag = Proxy :: Proxy "except"

throw :: forall e r a. e -> Eff (Except e + r) a
throw = throwAt tag

throwAt
  :: forall tag e r er a
   . Row.Cons tag (ExceptF e) r er
  => IsSymbol tag
  => Proxy tag
  -> e
  -> Eff er a
throwAt ptag error = Eff.unsafeMkFromAff (throwError (foreign_mkCustomError { tag: reflectSymbol ptag, value: error }))

catch
  :: forall e r a
   . Eff (Except e + r) a
  -> (e -> Eff r a)
  -> Eff r a
catch = catchAt tag

catchAt
  :: forall tag e r er a
   . Row.Cons tag (ExceptF e) r er
  => IsSymbol tag
  => Proxy tag
  -> Eff er a
  -> (e -> Eff r a)
  -> Eff r a
catchAt ptag (Eff.UnsafeMk m) handler = Eff.UnsafeMk \environment ->
  m (unsafeCoerce environment) `catchError` \error -> case parseCustomError ptag error of
    Just e -> Eff.un (handler e) environment
    Nothing -> throwError error

note :: forall e r a. e -> Maybe a -> Eff (Except e + r) a
note = noteAt tag

noteAt
  :: forall tag e r er a
   . Row.Cons tag (ExceptF e) r er
  => IsSymbol tag
  => Proxy tag
  -> e
  -> Maybe a
  -> Eff er a
noteAt ptag error = case _ of
  Nothing -> throwAt ptag error
  Just x -> pure x

rethrow :: forall e r a. Either e a -> Eff (Except e + r) a
rethrow = rethrowAt tag

rethrowAt
  :: forall tag e r er a
   . Row.Cons tag (ExceptF e) r er
  => IsSymbol tag
  => Proxy tag
  -> Either e a
  -> Eff er a
rethrowAt ptag = case _ of
  Left error -> throwAt ptag error
  Right x -> pure x

try :: forall e r a. Eff (Except e + r) a -> Eff r (Either e a)
try = tryAt tag

tryAt
  :: forall tag e r er a
   . Row.Cons tag (ExceptF e) r er
  => IsSymbol tag
  => Proxy tag
  -> Eff er a
  -> Eff r (Either e a)
tryAt ptag m = catchAt ptag (Right <$> m) (pure <<< Left)

foreign import foreign_mkCustomError :: forall e. { tag :: String, value :: e } -> Error

foreign import foreign_parseCustomError :: forall e. { tag :: String, error :: Error } -> Nullable e

parseCustomError :: forall tag e. IsSymbol tag => Proxy tag -> Error -> Maybe e
parseCustomError ptag error = Nullable.toMaybe (foreign_parseCustomError { tag: reflectSymbol ptag, error })
