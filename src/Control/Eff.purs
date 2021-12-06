{-|
Module     : Control.Eff
Maintainer : Mohammad Hasani (the-dr-lazy.github.io) <the-dr-lazy@pm.me>
Copyright  : (c) 2021 Effecful
License    : MPL 2.0

This Source Code Form is subject to the terms of the Mozilla Public
License, version 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Control.Eff
  ( module Control.Eff.Algebra
  , Environment
  , Action(..)
  , Eff(..)
  , mk
  , un
  , unsafeMkFromAff
  , interpret
  , intercept
  , reinterpret
  , expand
  , send
  , run
  ) where

import Prelude

import Control.Eff.Algebra
import Control.Eff.Algebra as Eff
import Control.Eff.Variant as Eff
import Control.Eff.Variant as Eff.Variant
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff)
import Type.Proxy (Proxy)
import Type.Row as Row
import Unsafe.Coerce (unsafeCoerce)

newtype Eff :: Row Eff.Algebra -> Type -> Type
newtype Eff r a = UnsafeMk (Environment r -> Aff a)

type role Eff nominal representational

instance Functor (Eff r) where
  map f (UnsafeMk m) = UnsafeMk \ref -> map f (m ref)

instance Apply (Eff r) where
  apply (UnsafeMk f) (UnsafeMk m) = UnsafeMk \ref -> apply (f ref) (m ref)

instance Applicative (Eff r) where
  pure = unsafeMkFromAff <<< pure

instance Bind (Eff r) where
  bind (UnsafeMk m) f = UnsafeMk \ref -> do
    x <- m ref
    un (f x) ref

instance Monad (Eff r)

type Environment r = Eff.Variant r (Eff r) ~> Action r

data Action r a = Intercept (Environment r) (Eff r a) | Interpret (Eff r a) | Perform (Aff a)

mk :: forall r. Eff.Variant r (Eff r) ~> Eff r
mk f = UnsafeMk \environment -> do
  case environment f of
    Perform m -> m
    Interpret (UnsafeMk m) -> m environment
    Intercept env (UnsafeMk m) -> m env

unsafeMkFromAff :: forall r. Aff ~> Eff r
unsafeMkFromAff m = UnsafeMk \_ -> m

un :: forall r a. Eff r a -> Environment r -> Aff a
un (UnsafeMk m) = m

send
  :: forall tag f r fr a
   . IsSymbol tag
  => Functor (f (Eff fr))
  => Row.Cons tag f r fr
  => Proxy tag
  -> f (Eff fr) a
  -> Eff fr a
send ptag f = mk (Eff.Variant.inject ptag f)

interpret
  :: forall tag from r fromr
   . Row.Cons tag from r fromr
  => Functor (from (Eff fromr))
  => IsSymbol tag
  => Proxy tag
  -> (from (Eff fromr) ~> Eff fromr)
  -> Eff fromr
       ~> Eff r
interpret ptag interpreter (UnsafeMk m) =
  UnsafeMk \environment ->
    m (Eff.Variant.interpret ptag (Interpret <<< interpreter) (unsafeCoerce environment))

intercept
  :: forall tag from r fromr
   . Row.Cons tag from r fromr
  => IsSymbol tag
  => Functor (from (Eff fromr))
  => Proxy tag
  -> (from (Eff fromr) ~> Eff fromr)
  -> Eff fromr
       ~> Eff fromr
intercept ptag interceptor (UnsafeMk m) =
  UnsafeMk \environment ->
    m (Eff.Variant.intercept ptag (interceptor >>> Intercept environment) environment)

expand :: forall from r fromr. Row.Union from r fromr => Eff from ~> Eff fromr
expand = unsafeCoerce

reinterpret
  :: forall tag from r tor fromr fromtor
   . Row.Cons tag from r fromr
  => Row.Cons tag from tor fromtor
  => IsSymbol tag
  => Functor (from (Eff fromtor))
  => Proxy tag
  -> (from (Eff fromtor) ~> Eff fromtor)
  -> Eff fromr
       ~> Eff tor
reinterpret ptag reinterpreter m = interpret ptag reinterpreter (unsafeCoerce m)

run :: forall a. Eff () a -> Aff a
run (UnsafeMk m) = m Eff.Variant.empty
