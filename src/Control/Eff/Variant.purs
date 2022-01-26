{-|
Module     : Control.Eff.Variant
Maintainer : Mohammad Hasani (the-dr-lazy.github.io) <the-dr-lazy@pm.me>
Copyright  : (c) 2021-2022 Effecful
License    : MPL 2.0

This Source Code Form is subject to the terms of the Mozilla Public
License, version 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Control.Eff.Variant where

import Prelude
import Control.Eff.Algebra (Algebra)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy)
import Type.Row as Row
import Unsafe.Coerce (unsafeCoerce)

data Variant :: Row Algebra -> (Type -> Type) -> Type -> Type
data Variant r m a

newtype VariantRep :: Algebra -> (Type -> Type) -> Type -> Type
newtype VariantRep f m a = VariantRep
  { tag :: String
  , value :: f m a
  , map :: forall x y. (x -> y) -> f m x -> f m y
  }

instance Functor (Variant r m) where
  map f a =
    case coerceY a of
      VariantRep v -> coerceV $ VariantRep
        { tag: v.tag
        , value: v.map f v.value
        , map: v.map
        }
    where
    coerceY :: forall f a. Variant r m a -> VariantRep f m a
    coerceY = unsafeCoerce

    coerceV :: forall f a. VariantRep f m a -> Variant r m a
    coerceV = unsafeCoerce

inject
  :: forall tag f r fr m a
   . Row.Cons tag f r fr
  => IsSymbol tag
  => Functor (f m)
  => Proxy tag
  -> f m a
  -> Variant fr m a
inject ptag value = coerceV $ VariantRep { tag: reflectSymbol ptag, value, map }
  where
  coerceV :: VariantRep f m a -> Variant fr m a
  coerceV = unsafeCoerce

interpret
  :: forall tag f r fr m a b
   . Row.Cons tag f r fr
  => IsSymbol tag
  => Proxy tag
  -> (f m a -> b)
  -> (Variant r m a -> b)
  -> (Variant fr m a -> b)
interpret ptag f g r =
  case coerceY r of
    VariantRep v | v.tag == reflectSymbol ptag -> f v.value
    _ -> g (coerceR r)
  where
  coerceY :: Variant fr m a -> VariantRep f m a
  coerceY = unsafeCoerce

  coerceR :: Variant fr m a -> Variant r m a
  coerceR = unsafeCoerce

intercept
  :: forall tag f r fr m a b
   . Row.Cons tag f r fr
  => IsSymbol tag
  => Proxy tag
  -> (f m a -> b)
  -> (Variant fr m a -> b)
  -> (Variant fr m a -> b)
intercept ptag f g r =
  case coerceY r of
    VariantRep v | v.tag == reflectSymbol ptag -> f v.value
    _ -> g (coerceR r)
  where
  coerceY :: Variant fr m a -> VariantRep f m a
  coerceY = unsafeCoerce

  coerceR :: Variant fr m a -> Variant fr m a
  coerceR = unsafeCoerce

reinterpret
  :: forall tag tag' f g r fr gr m a b
   . Row.Cons tag f r fr
  => Row.Cons tag' g r gr
  => IsSymbol tag
  => Proxy tag
  -> (f m a -> b)
  -> (Variant fr m a -> b)
  -> (Variant gr m a -> b)
reinterpret = unsafeCoerce 1

empty :: forall m a b. Variant () m a -> b
empty r = unsafeCrashWith case unsafeCoerce r of
  VariantRep v -> "Control.Eff.Variant: pattern match failure [" <> v.tag <> "]"
