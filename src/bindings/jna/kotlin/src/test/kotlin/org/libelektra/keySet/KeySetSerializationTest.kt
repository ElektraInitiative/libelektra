package org.libelektra.keySet

import kotlinx.serialization.Serializable
import kotlinx.serialization.SerializationException
import org.junit.Test
import org.libelektra.Key
import org.libelektra.KeySet
import org.libelektra.keySetExt.convert
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class KeySetSerializationTest {

    @Serializable
    data class SimpleTest(val name: String, val age: Int)

    @Serializable
    data class ComplexTest(val foo: String, val simple: SimpleTest)

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
    fun `convert with correct keySet but prefix and noise keys, returns correct object`() {
        val expected = SimpleTest("john", 25)
        val keySet = givenKeySetForSimpleTestWithPrefixAndOtherKeys(expected.name, expected.age)

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

    private fun givenKeySetForSimpleTest(name: String, age: Int) = KeySet.create(
            Key.create("/name", name),
            Key.create("/age", age)
    )

    private fun givenKeySetForSimpleTestWithPrefix(name: String, age: Int) = KeySet.create(
            Key.create("/prefix/name", name),
            Key.create("/prefix/age", age)
    )

    private fun givenKeySetForSimpleTestWithPrefixAndOtherKeys(name: String, age: Int) = KeySet.create(
            Key.create("/something/abc"),
            Key.create("/foo/bar"),
            Key.create("/bar"),
            Key.create("/prefix/name", name),
            Key.create("/other/thing"),
            Key.create("/prefix/age", age),
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
}
