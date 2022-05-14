package org.libelektra.keySetExt

import kotlinx.serialization.json.*
import org.libelektra.Key
import org.libelektra.KeySet
import org.libelektra.keyExt.isNotEmpty

/**
 * Converts a given KeySet to a JSON object using kotlinx.serialization
 *
 * The keyName hierarchy is converted to a JSON hierarchy
 * Every "/" in a keyName corresponds to a new JSON object created
 * All values will be set as their string representation
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
 */
fun KeySet.toJson(): JsonElement {
    val keysByKeyName: Map<String, Key> = associateBy { it.name }
    return toJsonInternal(keysByKeyName).jsonObject[""] ?: emptyJson()
}

/**
 * Converts a given KeySet to a JSON object using kotlinx.serialization
 *
 * Only converts keys with the prefix [rootKeyName]
 *
 * For more details, see [toJson]
 *
 * @param rootKeyName key name starting with "/" to filter unnecessary keys
 */
fun KeySet.toJson(rootKeyName: String): JsonElement {
    val keysByKeyName: Map<String, Key> = associateBy { it.name }
    return toJsonInternal(keysByKeyName, rootKeyName)
}

private fun KeySet.toJsonInternal(keysByKeyName: Map<String, Key>, currentKeyName: String = ""): JsonElement {
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

private fun JsonObjectBuilder.putEntryAsProperty(entry: Map.Entry<String, JsonElement>) {
    put(
            entry.key.lastInKeyName(),
            entry.value.jsonObject.getValue(entry.key.lastInKeyName())
    )
}

private fun Map<String, Key>.findChildrenOnNextLevel(currentKeyName: String) = keys
        .filter {
            it.startsWith("$currentKeyName/")
        }
        .map {
            "$currentKeyName/" + it.removePrefix("$currentKeyName/").substringBefore("/")
        }

private fun Map<String, Key>.buildSimpleJsonFor(currentKeyName: String): JsonObject {
    val key = get(currentKeyName) ?: return emptyJson()
    return buildJsonObject {
        put(key.name.lastInKeyName(), key.string)
    }
}

private fun String.lastInKeyName() = substringAfterLast("/")

private fun emptyJson() = buildJsonObject { }
