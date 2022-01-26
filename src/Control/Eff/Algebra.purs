{-|
Module     : Control.Eff.Algebra
Maintainer : Mohammad Hasani (the-dr-lazy.github.io) <the-dr-lazy@pm.me>
Copyright  : (c) 2021-2022 Effecful
License    : MPL 2.0

This Source Code Form is subject to the terms of the Mozilla Public
License, version 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Control.Eff.Algebra (Algebra) where

type Algebra = (Type -> Type) -> Type -> Type
