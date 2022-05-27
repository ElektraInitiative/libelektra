package org.libelektra.keySet

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.SerializationException
import org.junit.Test
import org.libelektra.Key
import org.libelektra.KeySet
import org.libelektra.keyExt.toElektraArrayIndex
import org.libelektra.keySetExt.convert
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class KeySetConvertTest {

    @Serializable
    data class SimpleTest(val name: String, val age: Int)

    @Serializable
    data class ComplexTest(val foo: String, val simple: SimpleTest)

    @Serializable
    data class ListTest(val foo: String, val bar: List<String>)

    @Serializable
    data class ParentValueTest(val foo: String, @SerialName("parentValue") val parent: String)

    @Test
    fun `convert with correct keySet, returns correct object`() {
        val expected = SimpleTest("john", 25)
        val keySet = givenKeySetForSimpleTest(expected.name, expected.age)

        val converted = keySet.convert<SimpleTest>()

        assertEquals(expected, converted)
    }

    @Test
    fun `convert with correct keySet but prefix, returns correct object`() {
        val expected = SimpleTest("john", 25)
        val keySet = givenKeySetForSimpleTestWithPrefix(expected.name, expected.age)

        val converted = keySet.convert<SimpleTest>()

        assertEquals(expected, converted)
    }

    @Test
    fun `convert with correct keySet and noise keys, returns correct object`() {
        val expected = SimpleTest("john", 25)
        val keySet = givenKeySetForSimpleTestWithOtherKeys(expected.name, expected.age)

        val converted = keySet.convert<SimpleTest>()

        assertEquals(expected, converted)
    }

    @Test
    fun `convert with empty keySet, throws SerializationException`() {
        val keySet = givenEmptyKeySet()

        assertFailsWith<SerializationException> {
            keySet.convert<SimpleTest>()
        }
    }

    @Test
    fun `convert with missing key, throws SerializationException`() {
        val keySet = givenKeySetWithMissingKey()

        assertFailsWith<SerializationException> {
            keySet.convert<SimpleTest>()
        }
    }

    @Test
    fun `convert with wrong type, throws SerializationException`() {
        val keySet = givenKeySetWithWrongType()

        assertFailsWith<SerializationException> {
            keySet.convert<SimpleTest>()
        }
    }

    @Test
    fun `convert with complex type and correct keySet, returns complex type`() {
        val expectation = ComplexTest("value", SimpleTest("name", 12))
        val keySet = givenComplexTypeKeySet(expectation.foo, expectation.simple)

        val converted = keySet.convert<ComplexTest>()

        assertEquals(expectation, converted)
    }

    @Test
    fun `convert with list type and correct keySet, returns list type`() {
        val expectation = ListTest("foo", listOf("bar1", "bar2"))
        val keySet = givenListTypeKeySet(expectation.foo, expectation.bar)

        val converted = keySet.convert<ListTest>()

        assertEquals(expectation, converted)
    }

    @Test
    fun `convert to list and correct keySet, returns list`() {
        val expectation = listOf("bar1", "bar2")
        val keySet = givenListKeySet(expectation)

        val converted = keySet.convert<List<String>>()

        assertEquals(expectation, converted)
    }

    @Test
    fun `convert to list of custom objects and correct keySet, returns list`() {
        val expectation = listOf(
                SimpleTest("philipp", 24),
                SimpleTest("markus", 24),
        )
        val keySet = givenListOfSimpleObjectsKeySet(expectation)

        val converted = keySet.convert<List<SimpleTest>>()

        assertEquals(expectation, converted)
    }

    @Test
    fun `convert to simple object from given parentKey in array and correct keySet, returns correct simple object`() {
        val expectation = listOf(
                SimpleTest("philipp", 24),
                SimpleTest("markus", 24),
        )
        val keySet = givenListOfSimpleObjectsKeySet(expectation)

        val converted = keySet.convert<SimpleTest>("/simple/#0")

        assertEquals(expectation.first(), converted)
    }

    @Test
    fun `convert with parentValue, returns all properties including parentValue`() {
        val expectation = ParentValueTest("foo", "bar")
        val keySet = givenKeySetForParentValueTest(expectation)

        val converted = keySet.convert<ParentValueTest>()

        assertEquals(expectation, converted)
    }

    private fun givenKeySetForSimpleTest(name: String, age: Int) = KeySet.create(
            Key.create("/name", name),
            Key.create("/age", age)
    )

    private fun givenKeySetForSimpleTestWithPrefix(name: String, age: Int) = KeySet.create(
            Key.create("/prefix/name", name),
            Key.create("/prefix/age", age)
    )

    private fun givenKeySetForSimpleTestWithOtherKeys(name: String, age: Int) = KeySet.create(
            Key.create("/something/abc"),
            Key.create("/foo/bar"),
            Key.create("/bar"),
            Key.create("/name", name),
            Key.create("/other/thing"),
            Key.create("/age", age),
            Key.create("/another")
    )

    private fun givenEmptyKeySet() = KeySet.create()

    private fun givenKeySetWithMissingKey() = KeySet.create(
            Key.create("/name", "abc")
    )

    private fun givenKeySetWithWrongType() = KeySet.create(
            Key.create("/name", "abc"),
            Key.create("/age", "abc")
    )

    private fun givenComplexTypeKeySet(foo: String, simple: SimpleTest) = KeySet.create(
            Key.create("/foo", foo),
            Key.create("/simple/name", simple.name),
            Key.create("/simple/age", simple.age)
    )

    private fun givenListTypeKeySet(foo: String, bar: List<String>): KeySet {
        val keySet = KeySet.create(Key.create("/foo", foo))

        keySet.add(
                Key.create("/bar").setMeta("array", bar.indices.last.toElektraArrayIndex())
        )
        bar.forEachIndexed { index, s ->
            keySet.add(Key.create("/bar/${index.toElektraArrayIndex()}", s))
        }

        return keySet
    }

    private fun givenListKeySet(bar: List<String>): KeySet {
        val keySet = KeySet.create(
                Key.create("/bar").setMeta("array", bar.indices.last.toElektraArrayIndex())
        )

        bar.forEachIndexed { index, s ->
            keySet.add(Key.create("/bar/${index.toElektraArrayIndex()}", s))
        }

        return keySet
    }

    private fun givenListOfSimpleObjectsKeySet(simple: List<SimpleTest>): KeySet {
        val keySet = KeySet.create(
                Key.create("/simple").setMeta("array", simple.indices.last.toElektraArrayIndex())
        )

        simple.forEachIndexed { index, s ->
            val elektraIndex = index.toElektraArrayIndex()
            keySet.add(Key.create("/simple/$elektraIndex/name", s.name))
            keySet.add(Key.create("/simple/$elektraIndex/age", s.age))
        }

        return keySet
    }

    private fun givenKeySetForParentValueTest(expectation: ParentValueTest) = KeySet.create(
            Key.create("/parent", expectation.parent),
            Key.create("/parent/foo", expectation.foo)
    )
}
