package org.libelektra.keySet

import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import kotlinx.serialization.json.putJsonObject
import org.junit.Test
import org.libelektra.Key
import org.libelektra.KeySet
import org.libelektra.keySetExt.toJson
import kotlin.test.assertEquals

class KeySetJsonTest {

    @Test
    fun `toJson with empty keySet, returns empty JSON`() {
        val keySet = givenEmptyKeySet()

        val json = keySet.toJson()

        assertEquals(givenEmptyJson(), json)
    }

    @Test
    fun `toJson with single key, returns JSON with single prop`() {
        val keySet = givenEmptyKeySet().withKey("/test", "value")

        val json = keySet.toJson()

        assertEquals(buildJsonObject { put("test", "value") }, json)
    }

    @Test
    fun `toJson with flat keys, returns JSON with multiple keys`() {
        val keySet = givenFlatKeySet(3)

        val json = keySet.toJson()

        val expected = buildJsonObject {
            put("test1", "value1")
            put("test2", "value2")
            put("test3", "value3")
        }
        assertEquals(expected, json)
    }

    @Test
    fun `toJson with only nested keys, returns JSON with multiple nested keys with only last has value`() {
        val keySet = givenNestedKeySet(3)

        val json = keySet.toJson()

        val expected = buildJsonObject {
            putJsonObject("test1") {
                putJsonObject("test2") {
                    put("test3", "value3")
                }
            }
        }
        assertEquals(expected, json)
    }

    @Test
    fun `toJson with only nested keys with intermediate values, returns JSON with multiple nested keys with special parent-key values`() {
        val keySet = givenNestedKeySet(3, withIntermediateValues = true)

        val json = keySet.toJson()

        val expected = buildJsonObject {
            putJsonObject("test1") {
                put("parentValue", "value1")
                putJsonObject("test2") {
                    put("test3", "value3")
                    put("parentValue", "value2")
                }
            }
        }
        assertEquals(expected, json)
    }

    @Test
    fun `toJson with only nested keys and multiple leaves with values, returns JSON with multiple nested keys`() {
        val keySet = givenEmptyKeySet()
                .withKeys(
                        "/test1" to "",
                        "/test1/test2" to "",
                        "/test1/test2/test3" to "value3",
                        "/test1/test2/test4" to "value4",
                )

        val json = keySet.toJson()

        val expected = buildJsonObject {
            putJsonObject("test1") {
                putJsonObject("test2") {
                    put("test3", "value3")
                    put("test4", "value4")
                }
            }
        }
        assertEquals(expected, json)
    }

    @Test
    fun `toJson with flat and nested keys and only leaves have values, returns JSON with flat and nested keys with no special values`() {
        val keySet = givenNestedKeySet(3)
                .withKeys(
                        "/foo1" to "",
                        "/foo1/foo2" to "fooValue2",
                )

        val json = keySet.toJson()

        val expected = buildJsonObject {
            putJsonObject("test1") {
                putJsonObject("test2") {
                    put("test3", "value3")
                }
            }
            putJsonObject("foo1") {
                put("foo2", "fooValue2")
            }
        }
        assertEquals(expected, json)
    }

    @Test
    fun `toJson with flat and nested keys and intermediate values, returns JSON with flat and nested keys with special values`() {
        val keySet = givenNestedKeySet(3, withIntermediateValues = true)
                .withKeys(
                        "/foo1" to "fooValue1",
                        "/foo1/foo2" to "fooValue2",
                )

        val json = keySet.toJson()

        val expected = buildJsonObject {
            putJsonObject("test1") {
                put("parentValue", "value1")
                putJsonObject("test2") {
                    put("parentValue", "value2")
                    put("test3", "value3")
                }
            }
            putJsonObject("foo1") {
                put("parentValue", "fooValue1")
                put("foo2", "fooValue2")
            }
        }
        assertEquals(expected, json)
    }

    @Test
    fun `toJson(rootKey) with flat and nested keys, returns JSON with only keys from specified root`() {
        val keySet = givenNestedKeySet(3)
                .withKeys(
                        "/foo1" to "fooValue1",
                        "/foo1/foo2" to "fooValue2",
                )

        val json = keySet.toJson("/foo1")

        val expected = buildJsonObject {
            putJsonObject("foo1") {
                put("parentValue", "fooValue1")
                put("foo2", "fooValue2")
            }
        }
        assertEquals(expected, json)
    }

    @Test
    fun `toJson(rootKey) with flat  keys, returns JSON with single key`() {
        val keySet = givenFlatKeySet(5)

        val json = keySet.toJson("/test3")

        val expected = buildJsonObject {
                put("test3", "value3")
        }
        assertEquals(expected, json)
    }


    private fun givenEmptyKeySet() = KeySet.create()

    private fun KeySet.withKey(keyName: String, value: String) = append(Key.create(keyName, value))

    private fun KeySet.withKeys(vararg keyNamesToValues: Pair<String, String>): KeySet {
        keyNamesToValues.forEach { (key, value) -> add(Key.create(key, value)) }
        return this
    }

    private fun givenFlatKeySet(size: Int): KeySet {
        val keySet = KeySet.create()

        repeat(size) {
            keySet.add(Key.create("/test${it + 1}", "value${it + 1}"))
        }

        return keySet
    }

    private fun givenNestedKeySet(size: Int, withIntermediateValues: Boolean = false): KeySet {
        val keySet = KeySet.create()

        var currentKeyName = ""

        repeat(size) {
            val value = if (withIntermediateValues || it == size - 1) "value${it + 1}" else ""
            currentKeyName += "/test${it + 1}"
            keySet.add(Key.create(currentKeyName, value))
        }

        return keySet
    }

    private fun givenEmptyJson() = buildJsonObject { }
}

