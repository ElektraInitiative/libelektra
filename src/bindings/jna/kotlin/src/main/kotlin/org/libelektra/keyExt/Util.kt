package org.libelektra.keyExt

import org.libelektra.Key
import org.libelektra.ReadableKey
import kotlin.math.log10

fun Key.isEmpty() = (!isBinary && valueSize == 1) || valueSize < 1

fun Key.isNotEmpty() = !isEmpty()

/**
 * Represents a sequence of all key name parts starting with the top-level key
 */
val Key.nameParts: Sequence<String>
    get() = keyNameIterator().asSequence().drop(1)

fun <T> keyOf(name: String, value: T? = null, vararg metaKeys: Key): Key {
    return Key.create(name, value).apply {
        metaKeys.forEach {
            setMeta(it.name, it.string)
        }
    }
}

/**
 * @param metaName key name of the meta key prefixed with "meta:/", e.g. meta:/array
 * @return a readonly meta key or null if not found
 */
fun Key.getMetaOrNull(metaName: String): ReadableKey? = getMeta(metaName).orElse(null)

/**
 * @return  the last index of the elektra array if this key has an array meta key, i.e. meta:/array
 *
 * null, otherwise
 */
fun Key.lastArrayIndexOrNull(): Int? {
    return try {
        getMetaOrNull("meta:/array")?.parseIndex()
    } catch (e: NumberFormatException) {
        null
    }
}

fun ReadableKey.parseIndex() = this.string
        .removePrefix("#")
        .substringAfterLast("_")
        .toInt()

fun Int.toElektraArrayIndex(): String {
    require(this >= 0) { "Array index must be non-negative" }

    if (this == 0) {
        return "#0"
    }

    val underscores = "_".repeat(log10(toDouble()).toInt())
    return "#${underscores}${this}"
}
