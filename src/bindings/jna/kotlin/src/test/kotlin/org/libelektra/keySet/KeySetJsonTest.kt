package org.libelektra.keySet

import kotlinx.serialization.json.*
import org.junit.Test
import org.libelektra.Key
import org.libelektra.KeySet
import org.libelektra.keyExt.toElektraArrayIndex
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

    @Test
    fun `toJson with a primitive array, returns JSON containing array`() {
        val keySet = givenArrayKeySet(3)

        val json = keySet.toJson()

        val expected = buildJsonObject {
            putJsonArray("test") {
                add("0")
                add("1")
                add("2")
                add("3")
            }
        }
        assertEquals(expected, json)
    }

    @Test
    fun `toJson with a nested array, returns JSON containing array`() {
        val keySet = givenNestedArrayKeySet(3)

        val json = keySet.toJson()

        val expected = buildJsonObject {
            putJsonArray("test") {
                addJsonObject {
                    put("name0", "0")
                }
                addJsonObject {
                    put("name1", "1")
                }
                addJsonObject {
                    put("name2", "2")
                }
                addJsonObject {
                    put("name3", "3")
                }
            }
        }
        assertEquals(expected, json)
    }

    @Test
    fun `toJson(rootKey) with a nested array, returns JSON containing array`() {
        val keySet = givenNestedArrayKeySet(3)

        val json = keySet.toJson("/test")

        val expected = buildJsonArray {
            addJsonObject {
                put("name0", "0")
            }
            addJsonObject {
                put("name1", "1")
            }
            addJsonObject {
                put("name2", "2")
            }
            addJsonObject {
                put("name3", "3")
            }
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

    private fun givenArrayKeySet(lastIndex: Int): KeySet {
        val keySet = KeySet.create()

        val arrayKey = Key.create("/test").setMeta("array", lastIndex.toElektraArrayIndex())

        repeat(lastIndex + 1) {
            keySet.append(Key.create("/test/#$it", it))
        }

        return keySet.append(arrayKey)
    }

    private fun givenNestedArrayKeySet(lastIndex: Int): KeySet {
        val keySet = KeySet.create()

        val arrayKey = Key.create("/test").setMeta("array", "#$lastIndex")

        repeat(lastIndex + 1) {
            keySet.append(Key.create("/test/#$it/name$it", it))
        }

        return keySet.append(arrayKey)
    }
}

