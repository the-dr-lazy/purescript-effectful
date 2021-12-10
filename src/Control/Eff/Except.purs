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

data ExceptF :: Row Type -> Eff.Algebra
data ExceptF es m a

type Except :: Row Type -> Row Eff.Algebra -> Row Eff.Algebra
type Except es r = (except :: ExceptF es | r)

tag = Proxy :: Proxy "except"

throw
  :: forall tag e s es r a
   . Row.Cons tag e s es
  => IsSymbol tag
  => Proxy tag
  -> e
  -> Eff (Except es + r) a
throw ptag error = Eff.unsafeMkFromAff (throwError (foreign_mkCustomError { tag: reflectSymbol ptag, value: error }))

catch
  :: forall tag e s es r a
   . Row.Cons tag e s es
  => IsSymbol tag
  => Proxy tag
  -> Eff (Except es + r) a
  -> (e -> Eff (Except s + r) a)
  -> Eff (Except s + r) a
catch ptag (Eff.UnsafeMk m) handler = Eff.UnsafeMk \environment ->
  m (unsafeCoerce environment) `catchError` \error -> case parseCustomError ptag error of
    Just e -> Eff.un (handler e) environment
    Nothing -> throwError error

try
  :: forall tag e s es r a
   . Row.Cons tag e s es
  => IsSymbol tag
  => Proxy tag
  -> Eff (Except es + r) a
  -> Eff (Except s + r) (Either e a)
try ptag m = catch ptag (Right <$> m) (pure <<< Left)

note
  :: forall tag e s es r a
   . Row.Cons tag e s es
  => IsSymbol tag
  => Proxy tag
  -> e
  -> Maybe a
  -> Eff (Except es + r) a
note ptag error = case _ of
  Nothing -> throw ptag error
  Just x -> pure x

rethrow
  :: forall tag e s es r a
   . Row.Cons tag e s es
  => IsSymbol tag
  => Proxy tag
  -> Either e a
  -> Eff (Except es + r) a
rethrow ptag = case _ of
  Left error -> throw ptag error
  Right x -> pure x

run :: forall r. Eff (Except () + r) ~> Eff r
run = unsafeCoerce

foreign import foreign_mkCustomError :: forall es. { tag :: String, value :: es } -> Error

foreign import foreign_parseCustomError :: forall es. { tag :: String, error :: Error } -> Nullable es

parseCustomError :: forall tag es. IsSymbol tag => Proxy tag -> Error -> Maybe es
parseCustomError ptag error = Nullable.toMaybe (foreign_parseCustomError { tag: reflectSymbol ptag, error })
