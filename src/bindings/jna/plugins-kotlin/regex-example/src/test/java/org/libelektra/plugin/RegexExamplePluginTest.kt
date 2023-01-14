package org.libelektra.plugin

import junit.framework.TestCase.assertEquals
import org.junit.Before
import org.junit.Test
import org.libelektra.Key
import org.libelektra.KeySet
import org.libelektra.Plugin

const val KEY_NAME = "/this_is_some_test"
const val REGEX_MATCH_ALL = ""
const val REGEX_MATCH_UPPER_CASE = "[A-Z]*"
const val REGEX_MATCH_LOWER_CASE = "[a-z]*"
const val REGEX_MATCH_PART = "baum"
const val REGEX_MATCH_FULL = "^baum$"
const val REGEX_MATCH_BEGIN = "^baum"
const val REGEX_MATCH_END = "baum$"

const val VALUE_EXACT = "baum"
const val VALUE_WITH_PREFIX = "strauchbaum"
const val VALUE_WITH_SUFFIX = "baumstrauch"

val PARENT_KEY = Key.create("/parent")

class RegexExamplePluginTest {

    private lateinit var plugin: Plugin

    @Before
    fun setup() {
        plugin = RegexExamplePlugin()
    }

    @Test
    fun exactStringMatch() {
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(REGEX_MATCH_PART, VALUE_EXACT), PARENT_KEY))
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(REGEX_MATCH_PART, VALUE_WITH_PREFIX), PARENT_KEY))
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(REGEX_MATCH_PART, VALUE_WITH_SUFFIX), PARENT_KEY))
    }

    @Test
    fun emptyRegex() {
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(REGEX_MATCH_ALL, VALUE_EXACT), PARENT_KEY))
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(REGEX_MATCH_ALL, VALUE_WITH_PREFIX), PARENT_KEY))
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(REGEX_MATCH_ALL, VALUE_WITH_SUFFIX), PARENT_KEY))
    }

    @Test
    fun noRegex() {
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(null, VALUE_EXACT), PARENT_KEY))
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(null, VALUE_WITH_PREFIX), PARENT_KEY))
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(null, VALUE_WITH_SUFFIX), PARENT_KEY))
    }

    @Test
    fun upperCaseRegex() {
        assertEquals(Plugin.STATUS_ERROR, plugin.set(singleKeySet(REGEX_MATCH_UPPER_CASE, VALUE_EXACT), PARENT_KEY))
        assertEquals(
            Plugin.STATUS_ERROR,
            plugin.set(singleKeySet(REGEX_MATCH_UPPER_CASE, VALUE_WITH_PREFIX), PARENT_KEY)
        )
        assertEquals(
            Plugin.STATUS_ERROR,
            plugin.set(singleKeySet(REGEX_MATCH_UPPER_CASE, VALUE_WITH_SUFFIX), PARENT_KEY)
        )
    }

    @Test
    fun lowerCaseRegex() {
        assertEquals(Plugin.STATUS_ERROR, plugin.set(singleKeySet(REGEX_MATCH_LOWER_CASE, VALUE_EXACT), PARENT_KEY))
        assertEquals(
            Plugin.STATUS_ERROR,
            plugin.set(singleKeySet(REGEX_MATCH_LOWER_CASE, VALUE_WITH_PREFIX), PARENT_KEY)
        )
        assertEquals(
            Plugin.STATUS_ERROR,
            plugin.set(singleKeySet(REGEX_MATCH_LOWER_CASE, VALUE_WITH_SUFFIX), PARENT_KEY)
        )
    }

    @Test
    fun fullRegex() {
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(REGEX_MATCH_FULL, VALUE_EXACT), PARENT_KEY))
        assertEquals(Plugin.STATUS_ERROR, plugin.set(singleKeySet(REGEX_MATCH_FULL, VALUE_WITH_PREFIX), PARENT_KEY))
        assertEquals(Plugin.STATUS_ERROR, plugin.set(singleKeySet(REGEX_MATCH_FULL, VALUE_WITH_SUFFIX), PARENT_KEY))
    }

    @Test
    fun beginRegex() {
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(REGEX_MATCH_BEGIN, VALUE_EXACT), PARENT_KEY))
        assertEquals(Plugin.STATUS_ERROR, plugin.set(singleKeySet(REGEX_MATCH_BEGIN, VALUE_WITH_PREFIX), PARENT_KEY))
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(REGEX_MATCH_BEGIN, VALUE_WITH_SUFFIX), PARENT_KEY))
    }

    @Test
    fun endRegex() {
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(REGEX_MATCH_END, VALUE_EXACT), PARENT_KEY))
        assertEquals(Plugin.STATUS_SUCCESS, plugin.set(singleKeySet(REGEX_MATCH_END, VALUE_WITH_PREFIX), PARENT_KEY))
        assertEquals(Plugin.STATUS_ERROR, plugin.set(singleKeySet(REGEX_MATCH_END, VALUE_WITH_SUFFIX), PARENT_KEY))
    }
}

fun singleKeySet(regex: String?, value: String): KeySet {
    val key = Key.create(KEY_NAME, value)
    if (regex != null) {
        key.setMeta("check/validation", regex)
    }
    return KeySet.create(1, key)
}