package org.libelektra.keyExt

import org.libelektra.Key
import kotlin.reflect.KClass

/**
 * Sets the value of the key to the given argument in a type-safe way
 * Supported data types:
 * - String
 * - Boolean
 * - Byte
 * - Short
 * - Int
 * - Long
 * - Float
 * - Double
 *
 * When null is passed, key value is set to the empty string
 *
 * @param value new value of the key, or null to reset the key
 * @return the key object with the new value set
 * @throws IllegalArgumentException when type of value is not a primitive type or string
 */
inline fun <reified T : Any> Key.set(value: T?): Key {
    return if (value != null) {
        set(value, T::class)
    } else {
        setNull()
    }
}


/**
 * This function is needed for the type inferring function [set]
 *
 * @param value new non-null value of the key
 * @param clazz type of value
 * @return the key object with the new value set
 * @throws IllegalArgumentException when [clazz] is not a primitive type or string
 */
@PublishedApi
internal fun <T : Any> Key.set(value: T, clazz: KClass<T>): Key {
    when (clazz) {
        String::class -> string = value as String
        Boolean::class -> boolean = value as Boolean
        Byte::class -> byte = value as Byte
        Short::class -> short = value as Short
        Int::class -> int = value as Int
        Long::class -> long = value as Long
        Float::class -> float = value as Float
        Double::class -> double = value as Double
        else -> throw IllegalArgumentException("Key can only store primitive types, '${clazz.simpleName}' not supported")
    }

    return this
}
