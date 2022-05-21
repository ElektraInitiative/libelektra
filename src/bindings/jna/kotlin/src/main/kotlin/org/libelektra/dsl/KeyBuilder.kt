package org.libelektra.dsl

import org.libelektra.Key

class KeyBuilder {

    private lateinit var keyName: String
    private var value: Any? = null
    private var metaKeys: MutableList<Key> = mutableListOf()

    fun build(): Key {
        if (!::keyName.isInitialized) {
            throw IllegalStateException("Key name must be set")
        }

        return Key.create(keyName, value).apply {
            metaKeys.forEach {
                setMeta(it.name, it.string)
            }
        }
    }

    fun name(name: String) {
        this.keyName = name
    }

    fun value(value: Any?) {
        this.value = value
    }

    fun metaKey(name: String, value: String?) {
        metaKeys.add(Key.create(name, value))
    }
}

fun keyOf(initializer: KeyBuilder.() -> Unit): Key {
    return KeyBuilder().apply(initializer).build()
}
