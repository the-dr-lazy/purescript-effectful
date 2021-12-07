/*
 * Maintainer : Mohammad Hasani (the-dr-lazy.github.io) <the-dr-lazy@pm.me>
 * Copyright  : (c) 2021 Effecful
 * License    : MPL 2.0
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, version 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

exports.foreign_mkCustomError = function (r) {
    var e = new Error('Control.Eff.Except: unhandled exception (' + r.tag + ')')
    e._tag = r.tag
    e._value = r.value

    return e
}

exports.foreign_parseCustomError = function (r) {
    if (r.error._tag !== r.tag) {
        return null
    }

    return r.error._value
}
