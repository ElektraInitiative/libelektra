/**
 * @file
 *
 * @brief Source file for the units plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdberrors.h>
#include "units.h"
#include <regex.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int is_valid_key(Key * key, Key * parentKey)
{
	const Key * meta = keyGetMeta(key, "check/units");
	if (!meta) return 1;
	const char* value = keyString(key);
	const char * prefix = "(Y|Z|E|P|T|G|M|k|h|da|d|c|m|µ|n|p|f|a|z|y)";
    const char * unit = "?(m|g|s|A|K|mol|cd|rad|sr|Hz|N|Pa|J|W|C|V|F|Ω|Ohm|S|Wb|T|H|C|lm|lx|Bq|Gy|Sv|kat|l|L)";
    char regexString[256];
    snprintf(regexString, sizeof regexString, "%s%s%s%s%s", "^([0-9]+)\\.?([0-9]*)\\s*", prefix, "?", unit, "$");
	
	regex_t regex;
	regmatch_t offsets;
	int compile_failure = regcomp (&regex, regexString, REG_NOSUB | REG_EXTENDED | REG_NEWLINE);
	if (compile_failure) return -1;
	// regexec returns 0 on a successful match
	int match = !(regexec (&regex, value, 0, &offsets, 0));
	regfree (&regex);
	
	if(!match) {
		ELEKTRA_SET_ERRORF (214, parentKey, "Validation of key %s with value %s failed.", keyName (key), keyString (key));
	}

	return match;

}

int elektraUnitsGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/units"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/units", KEY_VALUE, "units plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/units/exports", KEY_END),
			       keyNew ("system/elektra/modules/units/exports/get", KEY_FUNC, elektraUnitsGet, KEY_END),
			       keyNew ("system/elektra/modules/units/exports/set", KEY_FUNC, elektraUnitsSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/units/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraUnitsSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/units");
		if (!meta) continue;
		int is_valid = is_valid_key (cur, parentKey);
		if (!is_valid) return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("units",
		ELEKTRA_PLUGIN_GET,	&elektraUnitsGet,
		ELEKTRA_PLUGIN_SET,	&elektraUnitsSet,
		ELEKTRA_PLUGIN_END);
}
