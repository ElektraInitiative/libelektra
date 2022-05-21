package org.libelektra.keySetExt

import kotlinx.serialization.json.*
import org.libelektra.Key
import org.libelektra.KeySet
import org.libelektra.keyExt.getLastArrayIndex
import org.libelektra.keyExt.isArray
import org.libelektra.keyExt.isNotEmpty
import org.libelektra.keyExt.toElektraArrayIndex

/**
 * Converts a given KeySet to a JSON object using kotlinx.serialization with support for arrays
 *
 * The keyName hierarchy is converted to a JSON hierarchy
 *
 * Every "/" in a keyName corresponds to a new JSON object created
 *
 * All values will be set as their string representation
 *
 * Arrays are supported and assume a valid array meta key
 *
 * Example:
 *
 *      KeySet (<keyName> to <value>):
 *      /test to ""
 *      /test/foo to "abc"
 *      /test/bar to 5
 *      /other to ""
 *      /other/foo to "bar"
 *
 *      Generated JSON:
 *      {
 *          "test": {
 *              "foo": "abc",
 *              "bar": "5"
 *          },
 *          "other": {
 *              "foo": "bar"
 *          }
 *      }
 *
 * Since there is a new JSON object created for every level in the keyName hierarchy, some values of intermediate keyNames would be lost.
 * In this case a special property ("parentValue") is added to the direct child object of the former key.
 *
 * For example:
 *
 *      KeySet (<keyName> to <value>):
 *      /test to "intermediate"
 *      /test/foo to "abc"
 *      /test/bar to 5
 *      /other to "intermediate"
 *      /other/foo to "bar"
 *
 *      Generated JSON:
 *      {
 *          "test": {
 *              "parentValue": "intermediate",
 *              "foo": "abc",
 *              "bar": "5"
 *          },
 *          "other": {
 *              "parentValue": "intermediate",
 *              "foo": "bar"
 *          }
 *      }
 *
 * Array example:
 *
 *      KeySet (<keyName> to <value>):
 *      /test to "" (meta:/array = #2)
 *      /test/#0 to "abc0"
 *      /test/#1 to "abc1"
 *      /test/#2 to "abc2"
 *
 *      Generated JSON:
 *      {
 *          "test": [
 *              "abc0",
 *              "abc1",
 *              "abc2"
 *          ]
 *      }
 */
fun KeySet.toJson(): JsonElement {
    val keysByKeyName: Map<String, Key> = associateBy { it.name }
    return toJsonInternal(keysByKeyName).jsonObject[""] ?: emptyJson()
}

/**
 * Converts a given KeySet to a JSON object using kotlinx.serialization
 *
 * Only converts keys with the prefix [parentKeyName]
 *
 * For more details, see [toJson]
 *
 * @param parentKeyName key name starting with "/" to filter unnecessary keys
 * @see [toJson]
 */
fun KeySet.toJson(parentKeyName: String): JsonElement {
    val keysByKeyName: Map<String, Key> = associateBy { it.name }
    return toJsonInternal(keysByKeyName, parentKeyName)
}

private fun KeySet.toJsonInternal(keysByKeyName: Map<String, Key>, currentKeyName: String = ""): JsonElement {
    if (keysByKeyName[currentKeyName]?.isArray() == true) {
        return buildJsonArrayInternal(keysByKeyName, currentKeyName)
    }

    val directChildrenKeyNames = keysByKeyName.findChildrenOnNextLevel(currentKeyName)
    if (directChildrenKeyNames.isEmpty()) {
        return keysByKeyName.buildSimpleJsonFor(currentKeyName)
    }

    val mergedChildren = directChildrenKeyNames.associateWith { key -> toJsonInternal(keysByKeyName, key) }

    return buildJsonObject {
        putJsonObject(currentKeyName.lastInKeyName()) {
            mergedChildren.forEach {
                putEntryAsProperty(it)

                keysByKeyName[currentKeyName]?.let { parentKey ->
                    val valueOfParentKeyWouldGetLost = parentKey.isNotEmpty()
                    if (valueOfParentKeyWouldGetLost) {
                        put("parentValue", parentKey.string)
                    }
                }
            }
        }
    }
}

private fun KeySet.buildJsonArrayInternal(keysByKeyName: Map<String, Key>, arrayParentKeyName: String): JsonArray {
    val lastIndex = keysByKeyName.getValue(arrayParentKeyName).getLastArrayIndex()

    val arrayElements = (0..lastIndex).mapNotEmptyJson {
        toJsonInternal(keysByKeyName, "$arrayParentKeyName/${it.toElektraArrayIndex()}")
    }

    return buildJsonArray {
        arrayElements.forEachIndexed { index, json ->
            add(
                    json.jsonObject.getValue(index.toElektraArrayIndex())
            )
        }
    }
}

private fun JsonObjectBuilder.putEntryAsProperty(entry: Map.Entry<String, JsonElement>) {
    when (entry.value) {
        is JsonObject -> put(
                entry.key.lastInKeyName(),
                entry.value.jsonObject.getValue(entry.key.lastInKeyName())
        )
        else -> put(
                entry.key.lastInKeyName(),
                entry.value
        )
    }
}

private fun Map<String, Key>.findChildrenOnNextLevel(currentKeyName: String) = keys
        .filter {
            it.startsWith("$currentKeyName/")
        }
        .map {
            "$currentKeyName/" + it.removePrefix("$currentKeyName/").substringBefore("/")
        }
        .toSet()

private fun Map<String, Key>.buildSimpleJsonFor(currentKeyName: String): JsonObject {
    val key = get(currentKeyName) ?: return emptyJson()
    return buildJsonObject {
        put(key.name.lastInKeyName(), key.string)
    }
}

private fun String.lastInKeyName() = substringAfterLast("/")

private fun emptyJson() = buildJsonObject { }

private fun <T, R : JsonElement> Iterable<T>.mapNotEmptyJson(transform: (T) -> R) = map(transform).filterNot { it == emptyJson() }
