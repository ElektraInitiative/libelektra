package org.libelektra.keySetExt.serialformat

import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.InternalSerializationApi
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.descriptors.SerialKind
import kotlinx.serialization.descriptors.StructureKind
import kotlinx.serialization.encoding.CompositeDecoder
import kotlinx.serialization.internal.NamedValueDecoder
import org.libelektra.Key
import org.libelektra.KeySet
import org.libelektra.keyExt.get
import org.libelektra.keyExt.lastArrayIndexOrNull
import org.libelektra.keyExt.toElektraArrayIndex

/**
 * Decoder for KeySets
 *
 * Supports primitive values, strings, collections and maps
 *
 * @see [KeySetFormat]
 * @see [KeySetEncoder]
 */
@OptIn(InternalSerializationApi::class, ExperimentalSerializationApi::class)
internal class KeySetDecoder(
        val keySet: KeySet,
        parentKey: String?,
        private val arraySize: Int? = null,
        private val mapSize: Int? = null
) : NamedValueDecoder() {

    private val currentRootKey: String

    init {
        currentRootKey = if (parentKey != null) {
            parentKey
        } else {
            val rootKeys = keySet.map {
                val firstSlashIndex = it.name.indexOf("/")
                val secondSlashIndex = it.name.indexOf("/", firstSlashIndex + 1)

                if (secondSlashIndex < 0) {
                    it.name
                } else {
                    it.name.substring(0, secondSlashIndex)
                }
            }.toSet()

            if (rootKeys.size > 1) {
                ""
            } else {
                rootKeys.first()
            }
        }
    }

    private var elementIndex = 0

    private val isArray = arraySize != null
    private val isMap = mapSize != null

    private val potentialMapElements = keySet.toList().filter { it.name.startsWith(currentRootKey) }

    override fun decodeTaggedValue(tag: String): Any {
        error("Cannot decode non-primitive values")
    }

    private fun decodeTaggedKey(tag: String): Key {
        val keyName = if (tag == PARENT_KEY_VALUE_PROP) {
            ""
        } else {
            getKeyNameFrom(tag)
        }

        if (isMap) {
            return potentialMapElements[tag.toInt() / 2]
        }

        return keySet.lookup(currentRootKey + keyName).orElseThrow {
            NoSuchElementException("No value present for ${currentRootKey + keyName}")
        }
    }

    private fun getKeyNameFrom(tag: String): String {
        if (isArray) {
            return "/${tag.replaceDigitsWithElektraIndex().replace(".", "/")}"
        }

        return "/${tag.replace(".", "/")}"
    }

    override fun decodeTaggedBoolean(tag: String): Boolean {
        return decodeTaggedKey(tag).getDesiredValue(tag)
    }

    override fun decodeTaggedByte(tag: String): Byte {
        return decodeTaggedKey(tag).getDesiredValue(tag)
    }

    override fun decodeTaggedChar(tag: String): Char {
        return decodeTaggedKey(tag).getDesiredValue(tag)
    }

    override fun decodeTaggedDouble(tag: String): Double {
        return decodeTaggedKey(tag).getDesiredValue(tag)
    }

    override fun decodeTaggedEnum(tag: String, enumDescriptor: SerialDescriptor): Int {
        return decodeTaggedKey(tag).getDesiredValue(tag)
    }

    override fun decodeTaggedFloat(tag: String): Float {
        return decodeTaggedKey(tag).getDesiredValue(tag)
    }

    override fun decodeTaggedInt(tag: String): Int {
        return decodeTaggedKey(tag).getDesiredValue(tag)
    }

    override fun decodeTaggedLong(tag: String): Long {
        return decodeTaggedKey(tag).getDesiredValue(tag)
    }

    override fun decodeTaggedShort(tag: String): Short {
        return decodeTaggedKey(tag).getDesiredValue(tag)
    }

    override fun decodeTaggedString(tag: String): String {
        return decodeTaggedKey(tag).getDesiredValue(tag)
    }

    override fun decodeElementIndex(descriptor: SerialDescriptor): Int {
        if (!isArray && !isMap && elementIndex == descriptor.elementsCount) return CompositeDecoder.DECODE_DONE
        else if (isArray && elementIndex == arraySize) return CompositeDecoder.DECODE_DONE
        else if (isMap && elementIndex == mapSize?.times(2)) return CompositeDecoder.DECODE_DONE
        return elementIndex++
    }

    override fun beginStructure(descriptor: SerialDescriptor): KeySetDecoder {
        var newParentKey = currentRootKey
        var expectedArraySize: Int? = null
        var expectedMapSize: Int? = null

        if (startOfSubObject()) {
            newParentKey = if (isArray) {
                "$currentRootKey/${currentTag.replaceDigitsWithElektraIndex()}"
            } else {
                "$currentRootKey/$currentTag"
            }
        }

        if (subObjectIsListLike(descriptor.kind)) {
            val arrayRootKey = keySet.lookup(newParentKey).get()
            val lastArrayIndex = arrayRootKey.lastArrayIndexOrNull()
                    ?: throw IllegalStateException("Key ${arrayRootKey.name} was expected to be an array, but had no meta key 'array'")
            expectedArraySize = lastArrayIndex + 1
        }

        if (subObjectIsMapLike(descriptor.kind)) {
            expectedMapSize = keySet.toList().filter { it.name.startsWith(newParentKey) }.size
        }

        return KeySetDecoder(keySet, newParentKey, expectedArraySize, expectedMapSize)
    }

    private fun subObjectIsListLike(kind: SerialKind) = kind == StructureKind.LIST

    private fun subObjectIsMapLike(kind: SerialKind) = kind == StructureKind.MAP

    private fun startOfSubObject() = currentTagOrNull != null

    private fun String.replaceDigitsWithElektraIndex() = replace(Regex("\\d+")) {
        it.value.toInt().toElektraArrayIndex()
    }

    private inline fun <reified T> Key.getDesiredValue(tag: String): T {
        if (isMap) {
            return if (tag.toInt() % 2 == 0) {
                getTypedBaseName()
            } else {
                get()
            }
        }

        return get()
    }

    private inline fun <reified T> Key.getTypedBaseName(): T {
        return when (T::class) {
            String::class -> baseName as T
            Boolean::class -> baseName.toBoolean() as T
            Byte::class -> baseName.toByte() as T
            Short::class -> baseName.toShort() as T
            Int::class -> baseName.toInt() as T
            Long::class -> baseName.toLong() as T
            Float::class -> baseName.toFloat() as T
            Double::class -> baseName.toDouble() as T
            else -> throw IllegalArgumentException("Can only use primitive types as key name")
        }
    }

    companion object {
        private const val PARENT_KEY_VALUE_PROP = "parentValue"
    }
}
