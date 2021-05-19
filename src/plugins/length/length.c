/**
 * @file
 *
 * @brief Source for length plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdberrors.h>
#include <stdio.h>

#include "length.h"


static int validateKey (Key * key, Key * parentKey)
{
	const Key * meta = keyGetMeta (key, "check/length");
	if (!meta) return 1;
	int rc = 0;
	int c = 0;
	const char * length = keyString (meta);
	int i;
	sscanf (length, "%d", &i);

	const char * text = keyString (key);

	while (text[c] != '\0')
	{
		c++;
	}
	if (c <= i)
	{
		rc = true;
	}
	else
	{
		rc = false;
	}


	if (!rc)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Length check of key %s with value %s failed. Maximum length is %d but the given string is %d", keyName (key),
							keyString (key), i, c);
	}

	return rc;
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
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/length");
		if (!meta) continue;
		int rc = validateKey (cur, parentKey);
		if (!rc) return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraLengthSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/length");
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
