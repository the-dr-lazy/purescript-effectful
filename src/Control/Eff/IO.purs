{-|
Module     : Control.Eff.IO
Maintainer : Mohammad Hasani (the-dr-lazy.github.io) <the-dr-lazy@pm.me>
Copyright  : (c) 2021 Effecful
License    : MPL 2.0

This Source Code Form is subject to the terms of the Mozilla Public
License, version 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Control.Eff.IO (
  IOF(..), IO, tag, run, class Lift, lift) where

import Control.Eff (Eff)
import Control.Eff as Eff
import Control.Eff.Variant as Eff.Variant
import Prelude
import Type.Proxy
import Type.Row
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Unsafe.Coerce (unsafeCoerce)

tag = Proxy :: Proxy "io"

newtype IOF :: Eff.Algebra
newtype IOF m a = IO (Aff a)

derive newtype instance Functor (IOF m)

type IO :: Row Eff.Algebra -> Row Eff.Algebra
type IO r = (io :: IOF | r)

perform :: forall m r. IOF m ~> Eff.Action r
perform (IO m) = Eff.Perform m

run :: forall r. Eff (IO + r) ~> Eff r
run (Eff.UnsafeMk m) = Eff.UnsafeMk \environment ->
  m (Eff.Variant.interpret tag perform (unsafeCoerce environment))

class Lift m where
  lift :: forall r. m ~> Eff (IO + r)

instance Lift Effect where
  lift = Eff.send tag <<< IO <<< liftEffect

instance Lift Aff where
  lift = Eff.send tag <<< IO
