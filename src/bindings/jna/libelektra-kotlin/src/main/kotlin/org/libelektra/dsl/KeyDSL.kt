package org.libelektra.dsl

import org.libelektra.Key

/**
 * KeyDSL provides a DSL API for building Keys
 * It provides a way to instantiate complex keys in a readable way.
 * Should not be used directly, but implicitly by calling [keyOf]
 *
 * @param name the name of the key, e.g. user:/test
 */
class KeyDSL(private val name: String) {

    var value: Any? = null
    private val metaKeys: MutableList<Key> = mutableListOf()

    fun build(): Key {
        return Key.create(name, value).apply {
            metaKeys.forEach {
                setMeta(it.name, it.string)
            }
        }
    }

    fun metaKey(name: String, value: Any? = null) {
        require(name.startsWith("meta:/")) { "Meta keys must have name prefix 'meta:/'" }

        metaKeys.add(Key.create(name, value))
    }
}

/**
 * Builder function for keys
 *
 * @param name the name of the key, e.g. user:/test
 * @param initializer a block to set value and meta keys for the created key
 */
fun keyOf(name: String, initializer: KeyDSL.() -> Unit = {}): Key {
    return KeyDSL(name).apply(initializer).build()
}

/**
 * Constructs a new key
 *
 * @param name key name starting with / and an optional namespace, e.g. user:/foo
 * @param value optional value of a primitive type
 * @param metaKeys optional meta keys added to this key, meta keys must have a name starting with meta:/
 * @return the new key with given properties
 */
fun <T> keyOf(name: String, value: T? = null, vararg metaKeys: Key): Key {
    return Key.create(name, value).apply {
        metaKeys.forEach {
            setMeta(it.name, it.string)
        }
    }
}
