package org.libelektra.keySetExt.serialization

import kotlinx.serialization.*
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.descriptors.StructureKind
import kotlinx.serialization.encoding.CompositeEncoder
import kotlinx.serialization.internal.NamedValueEncoder
import kotlinx.serialization.modules.EmptySerializersModule
import kotlinx.serialization.modules.SerializersModule
import org.libelektra.KeySet
import org.libelektra.dsl.keyOf
import org.libelektra.keyExt.keyOf
import org.libelektra.keyExt.toElektraArrayIndex

/**
 * Encoder for KeySets
 *
 * Supports primitive values, strings, collections and maps
 *
 * @see [KeySetFormat]
 * @see [KeySetDecoder]
 */
@OptIn(ExperimentalSerializationApi::class, InternalSerializationApi::class)
internal class KeySetEncoder : NamedValueEncoder() {
    val keySet = KeySet.create()

    private var currentListDepth = 0
    private var currentMapDepth = 0
    private var currentMapKey: String? = null

    override val serializersModule: SerializersModule
        get() = EmptySerializersModule

    override fun encodeTaggedValue(tag: String, value: Any) {
        val keyName = tag.toKeyName()

        if (currentMapDepth > 0) {
            addKeyFromMap(keyName, value)
        } else {
            keySet.append(
                    keyOf(keyName, value)
            )
        }
    }

    private fun addKeyFromMap(keyName: String, value: Any) {
        if (currentMapKey == null) {
            currentMapKey = keyName.substringBeforeLast("/").plus("/$value")
            return
        }

        keySet.append(
                keyOf(currentMapKey!!, value)
        )
        currentMapKey = null
    }

    override fun beginCollection(descriptor: SerialDescriptor, collectionSize: Int): CompositeEncoder {
        return super.beginCollection(descriptor, collectionSize).also {
            if (descriptor.kind == StructureKind.LIST && collectionSize > 0) {
                keySet.append(
                        keyOf(currentTag.toKeyName()) {
                            metaKey("meta:/array", collectionSize.minus(1).toElektraArrayIndex())
                        }
                )
            }
        }
    }

    override fun beginStructure(descriptor: SerialDescriptor): CompositeEncoder {
        when (descriptor.kind) {
            StructureKind.LIST -> currentListDepth++
            StructureKind.MAP -> currentMapDepth++
            else -> {}
        }

        return super.beginStructure(descriptor)
    }

    override fun endEncode(descriptor: SerialDescriptor) {
        when (descriptor.kind) {
            StructureKind.LIST -> currentListDepth--
            StructureKind.MAP -> currentMapDepth--
            else -> {}
        }
        super.endEncode(descriptor)
    }

    private fun String.toKeyName(): String {
        if (currentListDepth == 0) {
            return "/$this".replace(".", "/")
        }

        return "/${replaceDigitsWithElektraIndex()}".replace(".", "/")
    }

    private fun String.replaceDigitsWithElektraIndex() = replace(Regex("\\d+")) {
        it.value.toInt().toElektraArrayIndex()
    }
}