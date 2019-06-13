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
#include <stdio.h>
#include <stdlib.h>

static char * deblank (char * input)
{
	kdb_unsigned_long_long_t i, j;
	char * output = input;
	for (i = 0, j = 0; i < strlen (input); i++, j++)
	{
		if (input[i] != ' ')
			output[j] = input[i];
		else
			j--;
	}
	output[j] = 0;
	return output;
}

static kdb_unsigned_long_long_t isValidKey (Key * key)
{

	const char * value = keyString (key);
	// else we manipulate the original
	char * tempval = elektraStrDup (value);

	kdb_unsigned_long_long_t ret;

	char * endPtr;
	// convert to long, if valid key, pointer should point to spaces or memory suffix like MB, GB
	ret = ELEKTRA_UNSIGNED_LONG_LONG_S (tempval, &endPtr, 10);

	// remove possibly occurring blanks
	deblank (endPtr);

	// calculate the factor based on the suffix of the mameory value, return 0 if there is no matching to indicate an error of the
	// function
	if (strcmp (endPtr, "KB") == 0)
	{
		return 1000;
	}
	if (strcmp (endPtr, "MB") == 0)
	{
		return 1000000;
	}
	if (strcmp (endPtr, "GB") == 0)
	{
		return 1000000000;
	}
	if (strcmp (endPtr, "TB") == 0)
	{
		return 1000000000000;
	}
	if (strcmp (endPtr, "PB") == 0)
	{
		return 1000000000000000;
	}
	if (strcmp (endPtr, "B") == 0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

// formatFactor is used to determine by which factor the value has to multiplied to normalize to bytes
static int elektraMemoryvalueConvertToByteString (Key * key, kdb_unsigned_long_long_t formatFactor)
{

	const char * str = keyString (key);
	keySetMeta (key, "origvalue", str);
	char * ptr;
	kdb_unsigned_long_long_t ret;
	kdb_unsigned_long_long_t normalizedMemVal;

	ret = ELEKTRA_UNSIGNED_LONG_LONG_S (str, &ptr, 10);

	// check if return value within bounds
	if (ret > UINT64_MAX / formatFactor)
	{
		return 1;
	}

	normalizedMemVal = ret * formatFactor;

	// convert back to string
	const int n = snprintf (NULL, 0, ELEKTRA_UNSIGNED_LONG_LONG_F, normalizedMemVal);
	char buf[n + 1];

	snprintf (buf, n + 1, ELEKTRA_UNSIGNED_LONG_LONG_F, normalizedMemVal);

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
			kdb_unsigned_long_long_t format = isValidKey (cur);

			if (format == 0)
			{
				ELEKTRA_SET_ERRORF (171, parentKey, "%s is not formatted properly!", keyString (cur));
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

		elektraMemoryvalueRestore (cur);
		kdb_unsigned_long_long_t format = isValidKey (cur);

		if (format == 0)
		{
			ELEKTRA_SET_ERRORF (171, parentKey, "%s is not formatted properly!", keyString (cur));
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
