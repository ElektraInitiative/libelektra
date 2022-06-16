package org.libelektra.keySet

import kotlinx.serialization.json.*
import org.junit.Test
import org.libelektra.dsl.keyOf
import org.libelektra.dsl.keySetOf
import org.libelektra.keySetExt.KeySetSerializer
import kotlin.test.assertEquals

class KeySetSerializationTest {

    private fun givenKeySet() = keySetOf(
            keyOf("/test", "value"),
            keyOf("/test/foo", 123),
            keyOf("/test/bar", 231.0),
            keyOf("/other", true),
    )

    private fun givenJson() = buildJsonObject {
            put("/test", "value")
            put("/test/foo", "123")
            put("/test/bar", "231.0")
            put("/other", "true")
    }

    @Test
    fun `serialize to JSON, should equal JSON string`() {
        val keySet = givenKeySet()

        val encoded = Json.encodeToJsonElement(KeySetSerializer(), keySet)

        assertEquals(givenJson(), encoded)
    }

    @Test
    fun `serialize to JSON and back, should be equal`() {
        val keySet = givenKeySet()

        val encoded = Json.encodeToString(KeySetSerializer(), keySet)

        val decoded = Json.decodeFromString(KeySetSerializer(), encoded)

        assertEquals(keySet, decoded)
    }
}
