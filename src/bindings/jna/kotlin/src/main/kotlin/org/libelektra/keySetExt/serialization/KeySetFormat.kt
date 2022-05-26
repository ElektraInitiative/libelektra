package org.libelektra.keySetExt.serialization

import kotlinx.serialization.*
import kotlinx.serialization.modules.EmptySerializersModule
import kotlinx.serialization.modules.SerializersModule
import org.libelektra.KeySet

/**
 * The main entry point to work with KeySet serialization
 * Defines functions for encoding and decoding KeySets.
 *
 * Supports custom data classes, collections and maps.
 * Data classes must be annotated with [Serializable]
 *
 * Example usage:
 * ```
 * @Serializable
 * data class User(val name: String, val age: Int)
 *
 * val user = User("john", 19)
 * val encoded = KeySetFormat.encodeToKeySet(user)
 *
 * val ks = keySetOf(
 *      keyOf("/user/name", "john"),
 *      keyOf("/user/age", 19)
 * )
 *
 * val decoded = KeySetFormat.decodeFromKeySet<User>(ks)
 *
 * check(ks == encoded)
 * check(user == decoded)
 * ```
 */
@OptIn(ExperimentalSerializationApi::class)
sealed class KeySetFormat(
        override val serializersModule: SerializersModule
) : SerialFormat {

    companion object Default : KeySetFormat(EmptySerializersModule)

    fun <T> encodeToKeySet(serializer: SerializationStrategy<T>, value: T): KeySet {
        val encoder = KeySetEncoder()
        encoder.encodeSerializableValue(serializer, value)
        return encoder.keySet
    }

    inline fun <reified T> encodeToKeySet(value: T): KeySet {
        try {
            return encodeToKeySet(serializer(), value)
        } catch (e: Exception) {
            throw SerializationException("Failed to encode KeySet", e)
        }
    }

    fun <T> decodeFromKeySet(keySet: KeySet, parentKey: String?, deserializer: DeserializationStrategy<T>): T {
        val decoder = KeySetDecoder(keySet, parentKey)
        return decoder.decodeSerializableValue(deserializer)
    }

    inline fun <reified T> decodeFromKeySet(keySet: KeySet, parentKey: String? = null): T {
        try {
            return decodeFromKeySet(keySet, parentKey, serializer())
        } catch (e: Exception) {
            throw SerializationException("Failed to decode KeySet", e)
        }
    }

}
