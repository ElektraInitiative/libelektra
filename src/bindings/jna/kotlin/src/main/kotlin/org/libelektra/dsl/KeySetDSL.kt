package org.libelektra.dsl

import org.libelektra.Key
import org.libelektra.KeySet

class KeySetDSL {

    private var keySet: KeySet = KeySet.create()

    fun build(): KeySet {
        return keySet
    }

    fun key(key: Key) {
        keySet.add(key)
    }
}

fun keySetOf(initializer: KeySetDSL.() -> Unit): KeySet {
    return KeySetDSL().apply(initializer).build()
}
