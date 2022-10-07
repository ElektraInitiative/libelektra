package org.libelektra.keyExt

import org.libelektra.Key
import org.libelektra.ReadableKey
import kotlin.math.log10

/**
 * A Key is considered empty when:
 * - The type is not binary and the value size is equal to 1
 * - The type is binary and the size is < 0
 *
 * @see [isNotEmpty]
 * @return whether the key has a value
 */
fun Key.isEmpty() = (!isBinary && valueSize == 1) || valueSize < 1

/**
 * A Key is considered empty when:
 * - The type is not binary and the value size is equal to 1
 * - The type is binary and the size is < 0
 *
 *
 * @see [isEmpty]
 * @return whether the key has no value
 */
fun Key.isNotEmpty() = !isEmpty()

/**
 * Represents a sequence of all key name parts starting with the top-level key
 */
val Key.nameParts: Sequence<String>
    get() = keyNameIterator().asSequence().drop(1)

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

/**
 * Parses the Elektra array index from an array meta key
 *
 * @receiver an array meta key with an array index as value, e.g. #2 or #_10
 * @return the array index as int
 * @throws NumberFormatException when the value of this key is not a valid array index
 */
fun ReadableKey.parseIndex() = this.string
        .removePrefix("#")
        .substringAfterLast("_")
        .toInt()

/**
 * Transforms a number to an Elektra array index
 *
 * @receiver a non-negative number
 * @return the number as array index, e.g. "#0" or "#_10"
 */
fun Int.toElektraArrayIndex(): String {
    require(this >= 0) { "Array index must be non-negative" }

    if (this == 0) {
        return "#0"
    }

    val underscores = "_".repeat(log10(toDouble()).toInt())
    return "#${underscores}${this}"
}
