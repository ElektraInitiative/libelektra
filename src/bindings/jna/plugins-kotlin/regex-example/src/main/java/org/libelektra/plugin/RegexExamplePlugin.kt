package org.libelektra.plugin

import org.libelektra.ErrorCode
import org.libelektra.Key
import org.libelektra.KeySet
import org.libelektra.Plugin

const val PLUGIN_NAME = "regex-example-plugin"

class RegexExamplePlugin : Plugin {
    override fun getName(): String {
        return PLUGIN_NAME
    }

    override fun open(config: KeySet, errorKey: Key): Int {
        throw UnsupportedOperationException()
    }

    override fun get(keySet: KeySet, parentKey: Key): Int {
        if (parentKey.isBelowOrSame(Key.create(Plugin.PROCESS_CONTRACT_ROOT))) {
            keySet.append(
                Key.create(
                    Plugin.PROCESS_CONTRACT_ROOT + "/infos",
                    "Kotlin example plugin which checks for regex in key values"
                )
            )
            keySet.append(Key.create(Plugin.PROCESS_CONTRACT_ROOT + "/infos/provides", "check"))
            keySet.append(Key.create(Plugin.PROCESS_CONTRACT_ROOT + "/infos/placements", "presetstorage"))
            keySet.append(Key.create(Plugin.PROCESS_CONTRACT_ROOT + "/infos/author", "@Eiskasten, @deoknats861"))
            keySet.append(Key.create(Plugin.PROCESS_CONTRACT_ROOT + "/infos/metadata", "check/validation"))
            keySet.append(
                Key.create(
                    Plugin.PROCESS_CONTRACT_ROOT + "/infos/description",
                    "Check if a link is reachable or is a valid link"
                )
            )
            keySet.append(Key.create(Plugin.PROCESS_CONTRACT_ROOT + "/infos/status", "preview"))
            keySet.append(Key.create(Plugin.PROCESS_CONTRACT_ROOT + "/exports/has/set", "1"))
            return Plugin.STATUS_SUCCESS
        }
        throw UnsupportedOperationException()
    }

    private fun invalidRegex(key: Key): Boolean {
        return key.getMeta("check/validation").map {
            !key.string.matches(it.string.toRegex())
        }.orElse(false)
    }

    override fun set(keySet: KeySet, parentKey: Key): Int {
        val invalidRegexKeys = keySet.filter(this::invalidRegex)
        invalidRegexKeys.forEach {
            val reason = it.getMeta("check/validation/message").map { k -> " Reason: $k" }.orElse("")
            parentKey.setError(
                ErrorCode.VALIDATION_SYNTACTIC,
                "The key '${it.name}' with value '${it.string}' does not confirm to its regular expression.$reason"
            )
        }
        return if (invalidRegexKeys.isEmpty()) Plugin.STATUS_SUCCESS else Plugin.STATUS_ERROR
    }

    override fun error(keySet: KeySet, parentKey: Key): Int {
        throw UnsupportedOperationException()
    }

    override fun close(parentKey: Key): Int {
        throw UnsupportedOperationException()
    }

}