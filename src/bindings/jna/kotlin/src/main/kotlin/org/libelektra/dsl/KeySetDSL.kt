package org.libelektra.dsl

import org.libelektra.Key
import org.libelektra.KeySet

/**
 * KeySetDSL provides a DSL API for building KeySets
 * It provides a way to instantiate complex KeySets in a readable way.
 * Should not be used directly, but implicitly by calling [keySetOf]
 *
 * @param key the key which should be added to the KeySet
 */
class KeySetDSL {

    private var keys: MutableSet<Key> = HashSet()

    fun build(): KeySet {
        return KeySet.create(*keys.toTypedArray())
    }

    fun key(key: Key) {
        keys.add(key)
    }

    fun <T> key(name: String, value: T? = null, vararg metaKeys: Key) {
        keys.add(
            Key.create(name, value).apply {
                metaKeys.forEach {
                    setMeta(it.name, it.string)
                }
            }
        )
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
