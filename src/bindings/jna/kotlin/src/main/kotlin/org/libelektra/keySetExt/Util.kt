package org.libelektra.keySetExt

import org.libelektra.Key
import org.libelektra.KeySet

fun keySetOf(vararg keys: Key): KeySet {
    return KeySet.create(*keys)
}
