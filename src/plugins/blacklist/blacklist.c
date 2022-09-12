/**
 * @file
 *
 * @brief Source for blacklist plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdberrors.h>
#include <stdio.h>

#include "blacklist.h"

static void blacklistValidValues (const ElektraKey * key, ElektraKeyset * validValues)
{

	const ElektraKey * maxKey = elektraKeyGetMeta (key, "check/blacklist");
	const char * max = elektraKeyString (maxKey);

	char elem[sizeof ("check/blacklist/") + ELEKTRA_MAX_ARRAY_SIZE];
	strcpy (elem, "check/blacklist/");
	char * indexStart = elem + sizeof ("check/blacklist/") - 1;

	kdb_long_long_t index = 0;
	elektraWriteArrayNumber (indexStart, index);
	while (strcmp (indexStart, max) <= 0)
	{
		const ElektraKey * blacklistKey = elektraKeyGetMeta (key, elem);
		const char * name = elektraKeyString (blacklistKey);
		ElektraKey * k = elektraKeyNew ("user:/0", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (kdb_unsigned_long_long_t), ELEKTRA_KEY_END);
		elektraKeySetBaseName (k, name);
		elektraKeysetAppendKey (validValues, k);
		++index;
		elektraWriteArrayNumber (indexStart, index);
	}
}

static bool elektraCheckBlacklist (const ElektraKey * key)
{

	ElektraKeyset * validValues = elektraKeysetNew (0, ELEKTRA_KS_END);

	blacklistValidValues (key, validValues);

	char * values = elektraStrDup (elektraKeyString (key));
	char * value = values;

	ElektraKey * valueKey = elektraKeyNew ("user:/0", ELEKTRA_KEY_END);

	elektraKeySetBaseName (valueKey, value);
	if (elektraKeysetLookup (validValues, valueKey, 0) != NULL)
	{
		elektraKeyDel (valueKey);
		elektraKeysetDel (validValues);
		elektraFree (values);
		return false;
	}

	elektraKeyDel (valueKey);
	elektraKeysetDel (validValues);
	elektraFree (values);

	return true;
}

static void elektraSetErrorBlacklist (Plugin * handle ELEKTRA_UNUSED, const ElektraKey * key, ElektraKey * errorKey)
{
	const ElektraKey * maxKey = elektraKeyGetMeta (key, "check/blacklist");
	const char * max = maxKey == NULL ? NULL : elektraKeyString (maxKey);

	char * errorMessage = elektraFormat (
		"The key '%s' with string: '%s' is not allowed\n"
		"Blacklisted values:",
		elektraKeyName (key), elektraKeyString (key));

	char elem[sizeof ("check/blacklist/") + ELEKTRA_MAX_ARRAY_SIZE];
	strcpy (elem, "check/blacklist/");
	char * indexStart = elem + sizeof ("check/blacklist/") - 1;

	kdb_long_long_t index = 0;
	elektraWriteArrayNumber (indexStart, index);
	while (strcmp (indexStart, max) <= 0)
	{
		const ElektraKey * blacklistKey = elektraKeyGetMeta (key, elem);
		const char * name = blacklistKey != NULL ? elektraKeyString (blacklistKey) : "";
		char * newErrorMessage = elektraFormat ("%s '%s'", errorMessage, name);
		elektraFree (errorMessage);
		errorMessage = newErrorMessage;
		++index;
		elektraWriteArrayNumber (indexStart, index);
	}

	ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, errorMessage);
	elektraFree (errorMessage);
}

bool elektraBlacklistValidateKey (Plugin * handle, ElektraKey * key, ElektraKey * errorKey)
{
	if (!elektraCheckBlacklist (key))
	{
		elektraSetErrorBlacklist (handle, key, errorKey);
		return false;
	}

	return true;
}


int elektraBlacklistGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/blacklist"))
	{
		ElektraKeyset * contract = elektraKeysetNew (
			30, elektraKeyNew ("system:/elektra/modules/blacklist", ELEKTRA_KEY_VALUE, "blacklist plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/blacklist/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/blacklist/exports/get", ELEKTRA_KEY_FUNC, elektraBlacklistGet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/blacklist/exports/set", ELEKTRA_KEY_FUNC, elektraBlacklistSet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/blacklist/exports/validateKey", ELEKTRA_KEY_FUNC, elektraBlacklistValidateKey, ELEKTRA_KEY_END),
#include ELEKTRA_README
			elektraKeyNew ("system:/elektra/modules/blacklist/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	for (elektraCursor it = 0; it < elektraKeysetGetSize (returned); ++it)
	{
		ElektraKey * cur = elektraKeysetAtCursor (returned, it);
		const ElektraKey * meta = elektraKeyGetMeta (cur, "check/blacklist");
		if (!meta) continue;
		if (!elektraCheckBlacklist (cur))
		{
			elektraSetErrorBlacklist (handle, cur, parentKey);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraBlacklistSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	for (elektraCursor it = 0; it < elektraKeysetGetSize (returned); ++it)
	{
		ElektraKey * cur = elektraKeysetAtCursor (returned, it);
		const ElektraKey * meta = elektraKeyGetMeta (cur, "check/blacklist");
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

