package org.libelektra.key

import org.junit.Test
import org.libelektra.Key
import org.libelektra.dsl.keyOf
import org.libelektra.keyExt.getMetaOrNull
import org.libelektra.keyExt.nameParts
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
            keyOf("meta:/meta1", "value1"),
            keyOf("meta:/meta2", "value2")
        )

        assertEquals(key.getMetaOrNull("meta:/meta1")!!.string, "value1")
        assertEquals(key.getMetaOrNull("meta:/meta2")!!.string, "value2")
    }

    @Test
    fun `forEachKeyName iterates correctly`() {
        val key = Key.create("/foo/bar/some/thing")

        val keyNames = key.nameParts.toList()

        assertEquals(listOf("foo", "bar", "some", "thing"), keyNames)
    }
}
