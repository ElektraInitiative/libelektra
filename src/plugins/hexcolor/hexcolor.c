/**
 * @file
 *
 * @brief Source for hexcolor plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "hexcolor.h"
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbtypes.h>
#include <regex.h>
#include <stdlib.h>

typedef enum
{
	HEX_INVALID,
	HEX_THREE,
	HEX_FOUR,
	HEX_SIX,
	HEX_EIGHT
} HexVariant;

static HexVariant is_valid_key (Key * key, Key * parentKey)
{
	const Key * meta = keyGetMeta (key, "check/hexcolor");
	if (!meta) return 1;
	const char * value = keyString (key);
	const char * regexString = "^#([0-9a-fA-F]{3,4}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})$";

	regex_t regex;
	regmatch_t offsets;
	int compile_failure = regcomp (&regex, regexString, REG_NOSUB | REG_EXTENDED | REG_NEWLINE);
	if (compile_failure) return -1;
	// regexec returns 0 on a successful match
	int match = !(regexec (&regex, value, 0, &offsets, 0));
	regfree (&regex);

	if (!match)
	{
		ELEKTRA_SET_ERRORF (214, parentKey, "Validation of key %s with value %s failed.", keyName (key), keyString (key));
		return HEX_INVALID;
	}

	int len = strlen (value);
	switch (len)
	{
	case 4:
		return HEX_THREE;
	case 5:
		return HEX_FOUR;
	case 7:
		return HEX_SIX;
	case 9:
		return HEX_EIGHT;
	default:
		return HEX_INVALID;
	}
}

static void elektraColorSetInteger (Key * key, kdb_unsigned_long_t c)
{
	kdb_octet_t colorBytes[] = { c >> 24, (c & 0x00ff0000) >> 16, (c & 0x0000ff00) >> 8, (c & 0x000000ff) };
	keySetBinary (key, colorBytes, 4);
}

static char * elektraColorExpand (const char * str, HexVariant hexVar)
{
	// Expand #abcd to #aabbccdd or #abc to #aabbccff
	char * expandedStr = (char *) elektraMalloc (10);

	if (hexVar == HEX_THREE || hexVar == HEX_FOUR)
	{
		expandedStr[0] = '#';

		for (size_t i = 1; i < strlen (str); i++)
		{
			expandedStr[2 * i - 1] = str[i];
			expandedStr[2 * i] = str[i];
		}
	}
	else
	{
		strcpy (expandedStr, str);
	}

	if (hexVar == HEX_THREE || hexVar == HEX_SIX)
	{
		expandedStr[7] = 'f';
		expandedStr[8] = 'f';
	}

	expandedStr[9] = '\0';

	return expandedStr;
}

static void elektraColorNormalizeHexString (Key * key, HexVariant hexVar)
{
	const char * str = keyString (key);
	keySetMeta (key, "origvalue", str);

	kdb_unsigned_long_t color;
	if (hexVar != HEX_EIGHT)
	{
		char * expandedStr = elektraColorExpand (str, hexVar);
		ELEKTRA_LOG_DEBUG ("Expanded %s to %s", str, expandedStr);
		color = ELEKTRA_UNSIGNED_LONG_LONG_S (expandedStr + 1, NULL, 16);
		elektraFree (expandedStr);
	}
	else
	{
		color = ELEKTRA_UNSIGNED_LONG_LONG_S (str + 1, NULL, 16);
	}
	elektraColorSetInteger (key, color);
}

static void elektraColorRestore (Key * key)
{
	const Key * orig = keyGetMeta (key, "origvalue");
	if (orig != NULL)
	{
		keySetString (key, keyString (orig));
	}
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

	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/hexcolor");
		if (!meta) continue;
		HexVariant hexVar = is_valid_key (cur, parentKey);
		// if (!is_valid) return ELEKTRA_PLUGIN_STATUS_ERROR;
		elektraColorNormalizeHexString (cur, hexVar);
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	;
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
		elektraColorRestore (cur);
		HexVariant hexVar = is_valid_key (cur, parentKey);
		if (hexVar == HEX_INVALID) return ELEKTRA_PLUGIN_STATUS_ERROR;
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
