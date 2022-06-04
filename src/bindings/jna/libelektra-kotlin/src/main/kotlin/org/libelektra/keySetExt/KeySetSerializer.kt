package org.libelektra.keySetExt

import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.KSerializer
import kotlinx.serialization.builtins.MapSerializer
import kotlinx.serialization.builtins.serializer
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import org.libelektra.Key
import org.libelektra.KeySet
import org.libelektra.dsl.keySetOf

/**
 * Basic serializer for KeySets
 *
 * Can be used to serialize KeySets in different formats, e.g. JSON
 *
 * Does NOT support metakeys
 */
@OptIn(ExperimentalSerializationApi::class)
class KeySetSerializer : KSerializer<KeySet> {
    override val descriptor: SerialDescriptor
        get() = SerialDescriptor("KeySet", delegateSerializer.descriptor)

    private val delegateSerializer = MapSerializer(String.serializer(), String.serializer())

    override fun serialize(encoder: Encoder, value: KeySet) {
        val keySetMap = value.associate { it.name to it.string }
        encoder.encodeSerializableValue(delegateSerializer, keySetMap)
    }

    override fun deserialize(decoder: Decoder): KeySet {
        val keySetMap = decoder.decodeSerializableValue(delegateSerializer)
        return keySetOf {
            keySetMap.forEach { (name, value) ->
                key(Key.create(name, value))
            }
        }
    }
}
