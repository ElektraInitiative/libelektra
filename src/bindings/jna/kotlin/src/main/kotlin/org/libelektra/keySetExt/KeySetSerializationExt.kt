package org.libelektra.keySetExt

import kotlinx.serialization.SerializationException
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.decodeFromJsonElement
import kotlinx.serialization.json.jsonObject
import org.libelektra.KeySet

/**
 * Converts a KeySet to a Kotlin data class using Kotlin Json Serialization
 *
 * The data class must be annotated with @Serializable
 *
 * The KeySet must contain all properties of the data class either on the root-level or one level below the root-level
 *
 * Simple Example:
 *
 *      data class User(val username: String, val password: String)
 *
 *      KeySet (<keyName> to <value>):
 *      /username to "jane"
 *      /password to "1234"
 *
 *      keySet.convert<User>()
 *      // User(username = "jane", password = "1234")
 *
 * Example with below root level:
 *
 *      KeySet (<keyName> to <value>):
 *      /user/username to "jane"
 *      /user/password to "1234"
 *
 *      keySet.convert<User>()
 *      // User(username = "jane", password = "1234")
 *
 * Example with nesting:
 *
 *      data class Employee(val user: User, val salary: Double)
 *
 *      KeySet (<keyName> to <value>):
 *      /user/username to "jane"
 *      /user/password to "1234"
 *      /salary to "4321.0"
 *
 *      keySet.convert<Employee>()
 *      // Employee(user = User(username = "jane", password = "1234"), salary = 4321.0)
 *
 * @throws SerializationException when decoding fails or the properties are not on root-level or one below root, see [Json.decodeFromJsonElement] for details
 * @return an object decoded from the JSON representation of this KeySet (see [KeySet.toJson]])
 */
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
