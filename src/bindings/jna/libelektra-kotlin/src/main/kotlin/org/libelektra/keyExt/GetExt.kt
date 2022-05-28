package org.libelektra.keyExt

import org.libelektra.Key
import kotlin.reflect.KClass

/**
 * @return value of this key as one of the following types: String, Boolean, Byte, Short, Int, Long, Float, Double
 * @throws IllegalArgumentException if a non-primitive type was requested
 * @throws NumberFormatException if a number type is requested, but the value is not a number
 */
inline fun <reified T : Any> Key.get(): T {
    return get(T::class)
}

/**
 * See [get]
 * @param clazz the requested type
 * @see [get]
 */
@Suppress("UNCHECKED_CAST")
@PublishedApi
internal fun <T : Any> Key.get(clazz: KClass<T>): T {
    return when (clazz) {
        String::class -> string as T
        Boolean::class -> boolean as T
        Byte::class -> byte as T
        Short::class -> short as T
        Int::class -> int as T
        Long::class -> long as T
        Float::class -> float as T
        Double::class -> double as T
        else -> throw IllegalArgumentException("Key can only return primitive types, '${clazz.simpleName}' not supported")
    }
}

/**
 * @return
 * - same as [get], if the string value is not empty
 * - null, otherwise
 * @see [get]
 */
inline fun <reified T : Any> Key.getOrNull(): T? = if (isNotEmpty()) get(T::class) else null
