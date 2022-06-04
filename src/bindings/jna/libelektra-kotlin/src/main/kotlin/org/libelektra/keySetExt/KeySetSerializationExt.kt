package org.libelektra.keySetExt

import kotlinx.serialization.SerializationException
import org.libelektra.KeySet
import org.libelektra.keySetExt.serialformat.KeySetFormat

/**
 * Converts a KeySet to a Kotlin data class using Kotlin Json Serialization with support for Collections
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
 * @param parentKey starting point for serialization of this KeySet, only keys below this key are considered for serialization
 * @throws SerializationException when decoding fails or the properties are not on root-level or one below root
 * @return an object decoded from this KeySet (see [KeySetFormat.decodeFromKeySet]])
 */
inline fun <reified T : Any> KeySet.convert(parentKey: String? = null): T {
    return KeySetFormat.decodeFromKeySet(this, parentKey)
}
