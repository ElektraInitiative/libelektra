package org.libelektra.keySet

import org.junit.Test
import org.libelektra.Key
import org.libelektra.dsl.keySetOf
import kotlin.test.assertEquals

class KeySetDSLTest {

    @Test
    fun `keySetOf with no key, returns empty keySet`() {
        val keySet = keySetOf {}

        assertEquals(true, keySet.isEmpty())
    }

    @Test
    fun `keySetOf with one key, returns keySet with one key`() {
        val keySet = keySetOf {
            key(Key.create("/key1", "value1"))
        }

        assertEquals(1, keySet.size)
        assertEquals("value1", keySet.lookup("/key1").get().string)
    }

    @Test
    fun `keySetOf with two keys, returns keySet with two keys`() {
        val keySet = keySetOf {
            key(Key.create("/key1", "value1"))
            key(Key.create("/key2", "value2"))
        }

        assertEquals(2, keySet.size)
        assertEquals("value1", keySet.lookup("/key1").get().string)
        assertEquals("value2", keySet.lookup("/key2").get().string)
    }

    @Test
    fun `keySetOf with one name-value pair, returns keySet with one key`() {
        val keySet = keySetOf {
            key("/key1") {
                value = "value1"
            }
        }

        assertEquals(1, keySet.size)
        assertEquals("value1", keySet.lookup("/key1").get().string)
    }

    @Test
    fun `keySetOf with two name-value pairs, returns keySet with two keys`() {
        val keySet = keySetOf {
            key("/key1") {
                value = "value1"
            }
            key("/key2") {
                value = "value2"
            }
        }

        assertEquals(2, keySet.size)
        assertEquals("value1", keySet.lookup("/key1").get().string)
        assertEquals("value2", keySet.lookup("/key2").get().string)
    }
}
