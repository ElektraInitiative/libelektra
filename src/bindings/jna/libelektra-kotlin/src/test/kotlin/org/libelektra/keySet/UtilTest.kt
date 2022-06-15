package org.libelektra.keySet

import org.junit.Test
import org.libelektra.Key
import org.libelektra.dsl.keySetOf
import org.libelektra.keySetExt.get
import org.libelektra.keySetExt.lookupOrThrow
import org.libelektra.keySetExt.lookupOrNull
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertNotNull
import kotlin.test.assertNull

class UtilTest {

    @Test
    fun `keySetOf with no key, returns empty keySet`() {
        val keySet = keySetOf()

        assertEquals(keySet.isEmpty(), true)
    }

    @Test
    fun `keySetOf with one key, returns keySet with one key`() {
        val keySet = keySetOf(
                Key.create("/key1", "value1")
        )

        assertEquals(keySet.size, 1)
        assertEquals(keySet.lookup("/key1").get().string, "value1")
    }

    @Test
    fun `keySetOf with two keys, returns keySet with two keys`() {
        val keySet = keySetOf(
                Key.create("/key1", "value1"),
                Key.create("/key2", "value2")
        )

        assertEquals(keySet.size, 2)
        assertEquals(keySet.lookup("/key1").get().string, "value1")
        assertEquals(keySet.lookup("/key2").get().string, "value2")
    }

    @Test
    fun `find in keySet with no keys, throws`() {
        val keySet = keySetOf()

        assertFailsWith<NoSuchElementException> {
            keySet.lookupOrThrow("/non/existing")
        }
    }

    @Test
    fun `find in keySet with wrong keyName, throws`() {
        val keySet = keySetOf(
                Key.create("/existing")
        )

        assertFailsWith<NoSuchElementException> {
            keySet.lookupOrThrow("/non/existing")
        }
    }

    @Test
    fun `find in keySet with correct keyName, returns key`() {
        val keySet = keySetOf(
                Key.create("/existing")
        )

        val key = keySet.lookupOrThrow("/existing")

        assertEquals("/existing", key.name)
    }

    @Test
    fun `findOrNull in keySet with no keys, returns null`() {
        val keySet = keySetOf()

        val key = keySet.lookupOrNull("/non/existing")

        assertNull(key)
    }

    @Test
    fun `findOrNull in keySet with wrong keyName, returns null`() {
        val keySet = keySetOf(
                Key.create("/existing")
        )

        val key = keySet.lookupOrNull("/non/existing")

        assertNull(key)
    }

    @Test
    fun `findOrNull in keySet with correct keyName, returns key`() {
        val keySet = keySetOf(
                Key.create("/existing")
        )

        val key = keySet.lookupOrNull("/existing")

        assertNotNull(key)
        assertEquals("/existing", key.name)
    }

    @Test
    fun `get operator in keySet with no keys, returns null`() {
        val keySet = keySetOf()

        val key = keySet["/non/existing"]

        assertNull(key)
    }

    @Test
    fun `get operator in keySet with wrong keyName, returns null`() {
        val keySet = keySetOf(
                Key.create("/existing")
        )

        val key = keySet["/non/existing"]

        assertNull(key)
    }

    @Test
    fun `get operator in keySet with correct keyName, returns key`() {
        val keySet = keySetOf(
                Key.create("/existing")
        )

        val key = keySet["/existing"]

        assertNotNull(key)
        assertEquals("/existing", key.name)
    }
}
