package org.libelektra.keyExt

import org.libelektra.Key
import java.util.*

fun Key.isEmpty() = string.isEmpty()

fun Key.isNotEmpty() = !isEmpty()

fun <T> keyOf(name: String, value: T?): Key {
    return Key.create(name, value)
}

fun <T> keyOf(name: String, value: T?, vararg metaKeys: Key): Key {
    return Key.create(name, value).apply {
        metaKeys.forEach {
            setMeta(it.name, it.string)
        }
    }
}

fun <T> Optional<T>.orNull(): T? = orElse(null)
