/**
 * @file
 *
 * @brief Source for hexcolor plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdberrors.h>
#include "hexcolor.h"
#include <regex.h>
#include <kdbhelper.h>

int is_valid_key(Key * key, Key * parentKey)
{
	const Key * meta = keyGetMeta(key, "check/hexcolor");
	if (!meta) return 1;
	const char* value = keyString(key);
	const char * regexString = "^#([0-9a-fA-F]{3}|[0-9a-fA-F]{6})$";
	
	regex_t regex;
	regmatch_t offsets;
	int compile_failure = regcomp (&regex, regexString, REG_NOSUB | REG_EXTENDED | REG_NEWLINE);
	if (compile_failure) return -1;
	// regexec returns 0 on a successful match
	int match = !(regexec (&regex, value, 0, &offsets, 0));
	regfree (&regex);
	
	if(!match) {
		ELEKTRA_SET_ERRORF (213, parentKey, "Validation of key %s with value %s failed.", keyName (key), keyString (key));
	}

	return match;

}

int elektraHexcolorGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/hexcolor"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/hexcolor", KEY_VALUE, "hexcolor plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/hexcolor/exports", KEY_END),
			       keyNew ("system/elektra/modules/hexcolor/exports/get", KEY_FUNC, elektraHexcolorGet, KEY_END),
			       keyNew ("system/elektra/modules/hexcolor/exports/set", KEY_FUNC, elektraHexcolorSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/hexcolor/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraHexcolorSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/hexcolor");
		if (!meta) continue;
		int is_valid = is_valid_key (cur, parentKey);
		if (!is_valid) return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("hexcolor",
		ELEKTRA_PLUGIN_GET,	&elektraHexcolorGet,
		ELEKTRA_PLUGIN_SET,	&elektraHexcolorSet,
		ELEKTRA_PLUGIN_END);
}
