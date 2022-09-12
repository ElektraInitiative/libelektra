/**
 * @file
 *
 * @brief Source for length plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include "length.h"
#include <kdbease.h>
#include <kdberrors.h>
#include <stdlib.h>


static bool validateKey (ElektraKey * key, ElektraKey * parentKey)
{
	const ElektraKey * meta = keyGetMeta (key, "check/length/max");
	if (meta == NULL)
	{
		return true;
	}

	kdb_unsigned_long_long_t max;
	if (!elektraKeyToUnsignedLongLong (meta, &max))
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
			parentKey, "Couldn't read check/length/max value '%s' on key '%s'. It should be a non-negative integer.",
			keyString (meta), keyName (key));
		return false;
	}

	// subtract nul-terminator, if string value
	size_t length = keyGetValueSize (key) - (keyIsString (key) ? 1 : 0);

	if (length > max)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
			parentKey,
			"Length check of key '%s' with value '%s' failed. Maximum length is " ELEKTRA_UNSIGNED_LONG_LONG_F
			" but the given string has length %zd",
			keyName (key), keyString (key), max, length);
		return false;
	}

	return true;
}

int elektraLengthGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/length"))
	{
		ElektraKeyset * contract =
			ksNew (30, keyNew ("system:/elektra/modules/length", ELEKTRA_KEY_VALUE, "length plugin waits for your orders", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/length/exports", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/length/exports/get", ELEKTRA_KEY_FUNC, elektraLengthGet, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/length/exports/set", ELEKTRA_KEY_FUNC, elektraLengthSet, ELEKTRA_KEY_END),

#include ELEKTRA_README

			       keyNew ("system:/elektra/modules/length/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		ksAppend (returned, contract);
		ksDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	ElektraKey * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		const ElektraKey * meta = keyGetMeta (cur, "check/length/max");
		if (!meta) continue;
		int rc = validateKey (cur, parentKey);
		if (!rc) return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraLengthSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	ElektraKey * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		const ElektraKey * meta = keyGetMeta (cur, "check/length/max");
		if (!meta) continue;
		int rc = validateKey (cur, parentKey);
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
