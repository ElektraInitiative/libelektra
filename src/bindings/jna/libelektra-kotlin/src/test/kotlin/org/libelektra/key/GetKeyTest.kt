package org.libelektra.key

import org.junit.Test
import org.libelektra.Key
import org.libelektra.keyExt.get
import org.libelektra.keyExt.getOrNull
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class GetKeyTest {

    /**
     * Tests the type inference and also serves as a usage example
     */
    @Test
    fun `get with string, returns correct value`() {
        val key = Key.create().setString("abc")

        val explicitType: String = key.get()
        val typeParam = key.get<String>()

        assertEquals(key.string, key.get())
        assertEquals(key.string, explicitType)
        assertEquals(key.string, typeParam)
    }

    @Test
    fun `get with boolean, returns correct value`() {
        val key = Key.create().setBoolean(true)
        assertEquals(key.boolean, key.get())
    }

    @Test
    fun `get with byte, returns correct value`() {
        val key = Key.create().setByte(1)
        assertEquals(key.byte, key.get())
    }

    @Test
    fun `get with short, returns correct value`() {
        val key = Key.create().setShort(1)
        assertEquals(key.short, key.get())
    }

    @Test
    fun `get with int, returns correct value`() {
        val key = Key.create().setInt(1)
        assertEquals(key.int, key.get())
    }

    @Test
    fun `get with long, returns correct value`() {
        val key = Key.create().setLong(1)
        assertEquals(key.long, key.get())
    }

    @Test
    fun `get with float, returns correct value`() {
        val key = Key.create().setFloat(1f)
        assertEquals(key.float, key.get())
    }

    @Test
    fun `get with double, returns correct value`() {
        val key = Key.create().setDouble(1.0)
        assertEquals(key.double, key.get())
    }

    @Test
    fun `get with non-primitive, throws IllegalArgumentException`() {
        val key = Key.create()
        assertFailsWith<IllegalArgumentException> {
            key.get<Key>()
        }
    }

    @Test
    fun `get with empty value, returns empty or false for string and boolean`() {
        val key = Key.create()

        assertEquals("", key.get())
        assertEquals(false, key.get())
    }

    @Test
    fun `get with empty value, throws NumberFormatException for number types`() {
        val key = Key.create()

        assertFailsWith<NumberFormatException> { key.get<Byte>() }
        assertFailsWith<NumberFormatException> { key.get<Short>() }
        assertFailsWith<NumberFormatException> { key.get<Int>() }
        assertFailsWith<NumberFormatException> { key.get<Long>() }
        assertFailsWith<NumberFormatException> { key.get<Float>() }
        assertFailsWith<NumberFormatException> { key.get<Double>() }
    }

    @Test
    fun `getOrNull with value, returns same as get`() {
        val key = Key.create().setString("abc")

        assertEquals(key.get<String>(), key.getOrNull())
    }

    @Test
    fun `getOrNull with empty value, returns null`() {
        val key = Key.create()

        assertEquals(null, key.getOrNull<String>())
    }
}
