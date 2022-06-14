package org.libelektra.key

import org.junit.Test
import org.libelektra.Key
import org.libelektra.exception.KeyStringValueException
import org.libelektra.keyExt.isEmpty
import org.libelektra.keyExt.set
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class SetKeyTest {

    private fun givenEmptyKey(): Key = Key.create()

    private fun Key.withString(value: String): Key = setString(value)

    @Test
    fun `set key with null, returns empty key`() {
        val key = givenEmptyKey().withString("test")

        key.set(null)

        assertEquals(true, key.isNull)
        assertFailsWith<KeyStringValueException> {
            key.string
        }
        assertEquals(true, key.isEmpty())
    }

    @Test
    fun `set key to boolean, correctly sets value`() {
        val key = givenEmptyKey()
        val value = true

        key.set(value)

        assertEquals(value, key.boolean)
    }

    @Test
    fun `set key to byte, correctly sets value`() {
        val key = givenEmptyKey()
        val value = 1.toByte()

        key.set(value)

        assertEquals(value, key.byte)
    }

    @Test
    fun `set key to short, correctly sets value`() {
        val key = givenEmptyKey()
        val value = 1.toShort()

        key.set(value)

        assertEquals(value, key.short)
    }

    @Test
    fun `set key to int, correctly sets value`() {
        val key = givenEmptyKey()
        val value = 1

        key.set(value)

        assertEquals(value, key.int)
    }

    @Test
    fun `set key to long, correctly sets value`() {
        val key = givenEmptyKey()
        val value = 1L

        key.set(value)

        assertEquals(value, key.long)
    }

    @Test
    fun `set key to float, correctly sets value`() {
        val key = givenEmptyKey()
        val value = 1f

        key.set(value)

        assertEquals(value, key.float)
    }

    @Test
    fun `set key to double, correctly sets value`() {
        val key = givenEmptyKey()
        val value = 1.0

        key.set(value)

        assertEquals(value, key.double)
    }

    @Test
    fun `set key to custom object, throws IllegalArgumentException`() {
        val key = givenEmptyKey()
        val value = 1 to ""

        assertFailsWith<IllegalArgumentException> {
            key.set(value)
        }
    }
}
