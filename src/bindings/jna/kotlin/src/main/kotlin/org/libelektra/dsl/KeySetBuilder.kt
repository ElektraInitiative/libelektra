package org.libelektra.dsl

import org.libelektra.Key
import org.libelektra.KeySet

class KeySetBuilder {

    private var keySet: KeySet = KeySet.create()

    fun build(): KeySet {
        return keySet
    }

    fun key(key: Key) {
        keySet.add(key)
    }
}

fun keySetOf(initializer: KeySetBuilder.() -> Unit): KeySet {
    return KeySetBuilder().apply(initializer).build()
}
