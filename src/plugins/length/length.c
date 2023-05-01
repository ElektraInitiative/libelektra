/**
 * @file
 *
 * @brief Source for length plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./length.h"
#include <elektra/core/errors.h>
#include <elektra/type/conversion.h>
#include <internal/utility/old_helper.h>
#include <stdlib.h>


static bool validateKey (Key * key, Key * parentKey, bool addWarningOnly)
{
	const Key * meta = keyGetMeta (key, "check/length/max");
	if (meta == NULL)
	{
		return true;
	}

	kdb_unsigned_long_long_t max;
	if (!elektraKeyToUnsignedLongLong (meta, &max))
	{
		if (addWarningOnly)
		{
			ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (
				parentKey, "Couldn't read check/length/max value '%s' on key '%s'. It should be a non-negative integer.",
				keyString (meta), keyName (key));
		}
		else
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				parentKey, "Couldn't read check/length/max value '%s' on key '%s'. It should be a non-negative integer.",
				keyString (meta), keyName (key));
		}

		return false;
	}

	// subtract nul-terminator, if string value
	size_t length = keyGetValueSize (key) - (keyIsString (key) ? 1 : 0);

	if (length > max)
	{
		if (addWarningOnly)
		{
			ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (
				parentKey,
				"Length check of key '%s' with value '%s' failed. Maximum length is " ELEKTRA_UNSIGNED_LONG_LONG_F
				" but the given string has length %zd",
				keyName (key), keyString (key), max, length);
		}
		else
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				parentKey,
				"Length check of key '%s' with value '%s' failed. Maximum length is " ELEKTRA_UNSIGNED_LONG_LONG_F
				" but the given string has length %zd",
				keyName (key), keyString (key), max, length);
		}

		return false;
	}

	return true;
}

int elektraLengthGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/length"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/length", KEY_VALUE, "length plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/length/exports", KEY_END),
			       keyNew ("system:/elektra/modules/length/exports/get", KEY_FUNC, elektraLengthGet, KEY_END),
			       keyNew ("system:/elektra/modules/length/exports/set", KEY_FUNC, elektraLengthSet, KEY_END),

#include ELEKTRA_README

			       keyNew ("system:/elektra/modules/length/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	Key * cur;

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		cur = ksAtCursor (returned, it);
		const Key * meta = keyGetMeta (cur, "check/length/max");
		if (!meta) continue;
		validateKey (cur, parentKey, true);
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraLengthSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	Key * cur;

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		cur = ksAtCursor (returned, it);
		const Key * meta = keyGetMeta (cur, "check/length/max");
		if (!meta) continue;
		int rc = validateKey (cur, parentKey, false);
		if (!rc) return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
                return elektraPluginExport ("length",
                ELEKTRA_PLUGIN_GET, &elektraLengthGet,
                ELEKTRA_PLUGIN_SET, &elektraLengthSet,
                ELEKTRA_PLUGIN_END);
        }
