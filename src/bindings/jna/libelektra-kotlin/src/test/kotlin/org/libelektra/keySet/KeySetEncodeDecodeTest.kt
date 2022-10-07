package org.libelektra.keySet

import kotlinx.serialization.Serializable
import org.junit.Test
import org.libelektra.KeySet
import org.libelektra.dsl.keyOf
import org.libelektra.dsl.keySetOf
import org.libelektra.keySetExt.serialformat.KeySetFormat
import kotlin.test.assertEquals

class KeySetEncodeDecodeTest {

    @Serializable
    data class ComplexObject(val name: String, val list: List<Float>, val set: Set<Int>, val map: Map<Byte, Short>, val simpleObject: SimpleObject)

    @Serializable
    data class SimpleObject(val foo: Double)

    private fun givenAComplexObject() = ComplexObject(
            name = "name",
            list = listOf(111f, 222f, 333f),
            set = setOf(1, 2, 3, 4),
            map = mapOf(5.toByte() to 1, 2.toByte() to 9),
            simpleObject = SimpleObject(
                    foo = 123.0
            )
    )

    private fun givenAComplexKeySet() = keySetOf(
            keyOf("/complex/name", "name"),
            keyOf("/complex/list") {
                metaKey("meta:/array", "#2")
            },
            keyOf("/complex/list/#0", 111f),
            keyOf("/complex/list/#1", 222f),
            keyOf("/complex/list/#2", 333f),
            keyOf("/complex/set") {
                metaKey("meta:/array", "#3")
            },
            keyOf("/complex/set/#0", 1),
            keyOf("/complex/set/#1", 2),
            keyOf("/complex/set/#2", 3),
            keyOf("/complex/set/#3", 4),
            keyOf("/complex/map/5", 1),
            keyOf("/complex/map/2", 9),
            keyOf("/complex/simpleObject/foo", 123.0)
    )

    private fun givenComplexKeySetWithoutPrefix() = givenAComplexKeySet().map {
        keyOf(it.name.removePrefix("/complex"), it.string)
    }.fold(KeySet.create()) { acc, key ->
        acc.append(key)
    }

    @Test
    fun `encode to keySet, should equal expectation`() {
        val complexObject = givenAComplexObject()

        val encoded = KeySetFormat.encodeToKeySet(complexObject)

        assertEquals(givenComplexKeySetWithoutPrefix(), encoded)
    }

    @Test
    fun `encode to keySet below parentKey, should equal keySet with prefix`() {
        val complexObject = givenAComplexObject()

        val encoded = KeySetFormat.encodeToKeySet(complexObject, "/complex")

        assertEquals(givenAComplexKeySet(), encoded)
    }

    @Test
    fun `decode from keySet, should equal expectation`() {
        val complexKeySet = givenAComplexKeySet()

        val decoded = KeySetFormat.decodeFromKeySet<ComplexObject>(complexKeySet)

        assertEquals(givenAComplexObject(), decoded)
    }

    @Test
    fun `encode and decode, should be equal`() {
        val complexObject = givenAComplexObject()

        val encoded = KeySetFormat.encodeToKeySet(complexObject)

        val decoded = KeySetFormat.decodeFromKeySet<ComplexObject>(encoded)

        assertEquals(complexObject, decoded)
    }
}
