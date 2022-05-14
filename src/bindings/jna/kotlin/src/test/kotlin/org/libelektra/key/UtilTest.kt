package org.libelektra.key

import org.junit.Test
import org.libelektra.Key
import org.libelektra.keyExt.forEachKeyName
import org.libelektra.keyExt.keyOf
import org.libelektra.keyExt.orNull
import kotlin.test.assertEquals

class UtilTest {

    @Test
    fun `keyOf with name and value, returns correct key`() {
        val key = keyOf("/test", "1234")

        assertEquals(key.name, "/test")
        assertEquals(key.string, "1234")
    }

    @Test
    fun `keyOf with name and value and meta keys, returns correct key including meta keys`() {
        val key = keyOf(
            "/test", "1234",
            keyOf("/meta1", "value1"),
            keyOf("/meta2", "value2")
        )

        assertEquals(key.getMeta("/meta1").orNull()!!.string, "value1")
        assertEquals(key.getMeta("/meta2").orNull()!!.string, "value2")
    }

    @Test
    fun `forEachKeyName iterates correctly`() {
        val key = Key.create("/foo/bar/some/thing")

        val keyNames = mutableListOf<String>()
        key.forEachKeyName {
            keyNames.add(it)
        }

        assertEquals(listOf("\u0001", "foo", "bar", "some", "thing"), keyNames)
    }
}
