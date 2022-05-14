package org.libelektra.keySetExt

import kotlinx.serialization.SerializationException
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.decodeFromJsonElement
import kotlinx.serialization.json.jsonObject
import org.libelektra.KeySet

inline fun <reified T : Any> KeySet.convert(): T {
    val json = toJson()

    return try {
        Json.decodeFromJsonElement(json)
    } catch (e: SerializationException) {
        tryToDecodeAllTopLevelProperties(json) ?: throw e
    }
}

@PublishedApi
internal inline fun <reified T : Any> tryToDecodeAllTopLevelProperties(json: JsonElement): T? {
    return json.jsonObject.entries.asSequence()
            .mapNotNull {
                try {
                    Json.decodeFromJsonElement<T>(it.value)
                } catch (e: SerializationException) {
                    null
                }
            }
            .firstOrNull()
}
