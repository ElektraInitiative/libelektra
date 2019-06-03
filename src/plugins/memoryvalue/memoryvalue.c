/**
 * @file
 *
 * @brief memoryvalue plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#define _XOPEN_SOURCE
#include "memoryvalue.h"
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbtypes.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>


static kdb_unsigned_long_long_t is_valid_key (Key * key)
{

	const Key * meta = keyGetMeta (key, "check/memoryvalue");
	const char * pattern = "\\d* ?[B,KB,MB,GB,TB,PB]";
	const char * value = keyString (key);
	regmatch_t offsets;
	regex_t regex;
	int compile_failure;
	int match;

	if (!meta)
	{
		return 0;
	}

	compile_failure = regcomp (&regex, pattern, REG_NOSUB | REG_EXTENDED | REG_NEWLINE);

	if (compile_failure)
	{
		return 0;
	}

	match = !(regexec (&regex, value, 0, &offsets, 0));
	regfree (&regex);

	if (!match)
	{
		return 0;
	}
	printf ("value %s", value);


	if (strstr (value, "KB") != NULL)
	{
		return 1000;
	}
	if (strstr (value, "MB") != NULL)
	{
		return 1000000;
	}
	if (strstr (value, "GB") != NULL)
	{
		return 1000000000;
	}
	if (strstr (value, "TB") != NULL)
	{
		return 1000000000000;
	}
	if (strstr (value, "PB") != NULL)
	{
		return 1000000000000000;
	}
	if (strstr (value, "B") != NULL)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}


static int elektraMemoryvalueConvertToByteString (Key * key, kdb_unsigned_long_long_t formatFactor)
{

	const char * str = keyString (key);
	keySetMeta (key, "origvalue", str);
	char * ptr;
	long ret;
	kdb_unsigned_long_long_t normalizedMemVal;

	ret = strtoll (str, &ptr, 10);

	// check if return value within bounds
	if (ret > UINT64_MAX / formatFactor)
	{
		return 1;
	}

	normalizedMemVal = ret * formatFactor;

	// convert back to string
	const int n = snprintf (NULL, 0, "%llu", normalizedMemVal);
	char buf[n + 1];

	snprintf (buf, n + 1, "%llu", normalizedMemVal);

	keySetString (key, buf);
	return 0;
}

static void elektraMemoryvalueRestore (Key * key)
{
	const Key * oldval = keyGetMeta (key, "origvalue");
	if (oldval != NULL)
	{
		keySetString (key, keyString (oldval));
	}
}


int elektraMemoryvalueGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/memoryvalue"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/memoryvalue", KEY_VALUE, "memoryvalue plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/memoryvalue/exports", KEY_END),
			keyNew ("system/elektra/modules/memoryvalue/exports/get", KEY_FUNC, elektraMemoryvalueGet, KEY_END),
			keyNew ("system/elektra/modules/memoryvalue/exports/set", KEY_FUNC, elektraMemoryvalueSet, KEY_END),
#include ELEKTRA_README
			keyNew ("system/elektra/modules/memoryvalue/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	Key * cur;
	int rc = 1;
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/memoryvalue");
		if (meta)
		{
			kdb_unsigned_long_long_t format = is_valid_key (cur);

			if (format == 0)
			{
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			elektraMemoryvalueConvertToByteString (cur, format);
		}
	}
	return rc;
}

int elektraMemoryvalueSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{

	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/memoryvalue");
		if (!meta)
		{
			continue;
		}
		printf ("contdd");

		elektraMemoryvalueRestore (cur);
		kdb_unsigned_long_long_t format = is_valid_key (cur);


		if (format == 0)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		int status = elektraMemoryvalueConvertToByteString (cur, format);

		if (status == 1)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}


Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("memoryvalue", ELEKTRA_PLUGIN_GET, &elektraMemoryvalueGet, ELEKTRA_PLUGIN_SET, &elektraMemoryvalueSet,
				    ELEKTRA_PLUGIN_END);
}
