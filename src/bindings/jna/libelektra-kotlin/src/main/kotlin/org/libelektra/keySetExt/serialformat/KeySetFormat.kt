package org.libelektra.keySetExt.serialformat

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
 *
 * @see [KeySetEncoder]
 * @see [KeySetDecoder]
 */
@OptIn(ExperimentalSerializationApi::class)
sealed class KeySetFormat(
        override val serializersModule: SerializersModule
) : SerialFormat {

    companion object Default : KeySetFormat(EmptySerializersModule())

    /**
     * Encodes any given value to a KeySet with the given serializer
     * The resulting KeySet will have keys under parentKey, if given
     *
     * @see [KeySetEncoder]
     *
     * @param T a serializable object, e.g. standard collections or classes annotated with [Serializable]
     * @param serializer strategy for encoding
     * @param value any value to encode, can be a class, list, map
     * @param parentKey the parent of all resulting keys in the keySet
     * @return a new KeySet
     */
    fun <T> encodeToKeySet(serializer: SerializationStrategy<T>, value: T, parentKey: String? = null): KeySet {
        val encoder = KeySetEncoder(parentKey)
        encoder.encodeSerializableValue(serializer, value)
        return encoder.keySet
    }

    /**
     * Encodes any given object to a KeySet
     * The resulting KeySet will have keys under parentKey, if given
     *
     * @see [KeySetEncoder]
     *
     * @param T a serializable object, e.g. standard collections or classes annotated with [Serializable]
     * @param value any value to encode, can be a class, list, map
     * @param parentKey the parent of all resulting keys in the keySet
     * @return a new KeySet
     * @throws SerializationException when the serialization failed (type not serializable, ..)
     */
    inline fun <reified T> encodeToKeySet(value: T, parentKey: String? = null): KeySet {
        try {
            return encodeToKeySet(serializer(), value, parentKey)
        } catch (e: Exception) {
            throw SerializationException("Failed to encode KeySet", e)
        }
    }

    /**
     * Decodes a keySet into an object using the given deserializer
     * Will search for object properties below a given parentKey, if given
     * Otherwise, it will use the root level or the first found equal prefix
     *
     * @see [KeySetDecoder]
     *
     * @param T a serializable object, e.g. standard collections or classes annotated with [Serializable]
     * @param keySet the keySet to decode the object from
     * @param parentKey the optional parentKey where all the properties of the object are below
     * @param deserializer the deserialization strategy
     * @return an object where all properties are filled with values from the keySet
     */
    fun <T> decodeFromKeySet(keySet: KeySet, parentKey: String? = null, deserializer: DeserializationStrategy<T>): T {
        val decoder = KeySetDecoder(keySet, parentKey)
        return decoder.decodeSerializableValue(deserializer)
    }

    /**
     * Decodes a keySet into an object
     * Will search for object properties below a given parentKey, if given
     * Otherwise, it will use the root level or the first found equal prefix
     *
     * @see [KeySetDecoder]
     *
     * @param T a serializable object, e.g. standard collections or classes annotated with [Serializable]
     * @param keySet the keySet to decode the object from
     * @param parentKey the optional parentKey where all the properties of the object are below
     * @return an object where all properties are filled with values from the keySet
     * @throws SerializationException when the deserialization failed (value missing in keySet, type not serializable, ..)
     */
    inline fun <reified T> decodeFromKeySet(keySet: KeySet, parentKey: String? = null): T {
        try {
            return decodeFromKeySet(keySet, parentKey, serializer())
        } catch (e: Exception) {
            throw SerializationException("Failed to decode KeySet", e)
        }
    }

}
