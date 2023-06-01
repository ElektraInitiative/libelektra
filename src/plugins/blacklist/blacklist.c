/**
 * @file
 *
 * @brief Source for blacklist plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <elektra/core/errors.h>
#include <internal/utility/compare.h>
#include <internal/utility/array.h>
#include <internal/utility/format.h>
#include <internal/utility/alloc.h>
#include <stdio.h>

#include "./blacklist.h"

static void blacklistValidValues (const Key * key, KeySet * validValues)
{

	const Key * maxKey = keyGetMeta (key, "check/blacklist");
	const char * max = keyString (maxKey);

	char elem[sizeof ("check/blacklist/") + ELEKTRA_MAX_ARRAY_SIZE];
	strcpy (elem, "check/blacklist/");
	char * indexStart = elem + sizeof ("check/blacklist/") - 1;

	kdb_long_long_t index = 0;
	elektraWriteArrayNumber (indexStart, index);
	while (strcmp (indexStart, max) <= 0)
	{
		const Key * blacklistKey = keyGetMeta (key, elem);
		const char * name = keyString (blacklistKey);
		Key * k = keyNew ("user:/0", KEY_BINARY, KEY_SIZE, sizeof (kdb_unsigned_long_long_t), KEY_END);
		keySetBaseName (k, name);
		ksAppendKey (validValues, k);
		++index;
		elektraWriteArrayNumber (indexStart, index);
	}
}

static bool elektraCheckBlacklist (const Key * key)
{

	KeySet * validValues = ksNew (0, KS_END);

	blacklistValidValues (key, validValues);

	char * values = elektraStrDup (keyString (key));
	char * value = values;

	Key * valueKey = keyNew ("user:/0", KEY_END);

	keySetBaseName (valueKey, value);
	if (ksLookup (validValues, valueKey, 0) != NULL)
	{
		keyDel (valueKey);
		ksDel (validValues);
		elektraFree (values);
		return false;
	}

	keyDel (valueKey);
	ksDel (validValues);
	elektraFree (values);

	return true;
}

static void elektraSetErrorBlacklist (Plugin * handle ELEKTRA_UNUSED, const Key * key, Key * errorKey)
{
	const Key * maxKey = keyGetMeta (key, "check/blacklist");
	const char * max = maxKey == NULL ? NULL : keyString (maxKey);

	char * errorMessage = elektraFormat (
		"The key '%s' with string: '%s' is not allowed\n"
		"Blacklisted values:",
		keyName (key), keyString (key));

	char elem[sizeof ("check/blacklist/") + ELEKTRA_MAX_ARRAY_SIZE];
	strcpy (elem, "check/blacklist/");
	char * indexStart = elem + sizeof ("check/blacklist/") - 1;

	kdb_long_long_t index = 0;
	elektraWriteArrayNumber (indexStart, index);
	while (strcmp (indexStart, max) <= 0)
	{
		const Key * blacklistKey = keyGetMeta (key, elem);
		const char * name = blacklistKey != NULL ? keyString (blacklistKey) : "";
		char * newErrorMessage = elektraFormat ("%s '%s'", errorMessage, name);
		elektraFree (errorMessage);
		errorMessage = newErrorMessage;
		++index;
		elektraWriteArrayNumber (indexStart, index);
	}

	ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, errorMessage);
	elektraFree (errorMessage);
}

bool elektraBlacklistValidateKey (Plugin * handle, Key * key, Key * errorKey)
{
	if (!elektraCheckBlacklist (key))
	{
		elektraSetErrorBlacklist (handle, key, errorKey);
		return false;
	}

	return true;
}


int elektraBlacklistGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/blacklist"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/blacklist", KEY_VALUE, "blacklist plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/blacklist/exports", KEY_END),
			keyNew ("system:/elektra/modules/blacklist/exports/get", KEY_FUNC, elektraBlacklistGet, KEY_END),
			keyNew ("system:/elektra/modules/blacklist/exports/set", KEY_FUNC, elektraBlacklistSet, KEY_END),
			keyNew ("system:/elektra/modules/blacklist/exports/validateKey", KEY_FUNC, elektraBlacklistValidateKey, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/blacklist/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
		const Key * meta = keyGetMeta (cur, "check/blacklist");
		if (!meta) continue;
		if (!elektraCheckBlacklist (cur))
		{
			elektraSetErrorBlacklist (handle, cur, parentKey);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraBlacklistSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
		const Key * meta = keyGetMeta (cur, "check/blacklist");
		if (!meta) continue;
		if (!elektraCheckBlacklist (cur))
		{
			elektraSetErrorBlacklist (handle, cur, parentKey);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
		return elektraPluginExport ("blacklist",
		ELEKTRA_PLUGIN_GET,	&elektraBlacklistGet,
		ELEKTRA_PLUGIN_SET,	&elektraBlacklistSet,
		ELEKTRA_PLUGIN_END);
	}

