package org.libelektra.keySet

import org.junit.Test
import org.libelektra.Key
import org.libelektra.keySetExt.keySetOf
import kotlin.test.assertEquals

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
        val keySet = keySetOf (
            Key.create("/key1", "value1"),
            Key.create("/key2", "value2")
        )

        assertEquals(keySet.size, 2)
        assertEquals(keySet.lookup("/key1").get().string, "value1")
        assertEquals(keySet.lookup("/key2").get().string, "value2")
    }
}
