package org.libelektra.dsl

import org.libelektra.Key
import org.libelektra.KeySet

/**
 * KeySetDSL provides a DSL API for building KeySets
 * It provides a way to instantiate complex KeySets in a readable way.
 * Should not be used directly, but implicitly by calling [keySetOf]
 */
class KeySetDSL {

    private var keys: MutableSet<Key> = HashSet()

    fun build(): KeySet {
        return KeySet.create(*keys.toTypedArray())
    }

    fun key(key: Key) {
        keys.add(key)
    }

    fun key(name: String, initializer: KeyDSL.() -> Unit) {
        keys.add(keyOf(name, initializer))
    }
}

/**
 * Builder function for KeySets
 *
 * @param initializer a block to keys for the created KeySet
 */
fun keySetOf(initializer: KeySetDSL.() -> Unit): KeySet {
    return KeySetDSL().apply(initializer).build()
}

/**
 * Constructs a KeySet with the given keys
 *
 * @param keys will be added to the new KeySet
 * @return KeySet containing all given keys
 */
fun keySetOf(vararg keys: Key): KeySet {
    return KeySet.create(*keys)
}
