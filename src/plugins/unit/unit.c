/**
 * @file
 *
 * @brief unit plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./unit.h"
#include <elektra/core/errors.h>
#include <elektra/type/types.h>
#include <internal/utility/old_helper.h>
#include <stdio.h>
#include <stdlib.h>

// @param input string, of which possibly occurring spaces are removed
static void deblank (char * input)
{
	int count = 0;

	for (int i = 0; input[i]; i++)
	{
		if (input[i] != ' ')
		{
			input[count++] = input[i];
		}
	}

	input[count] = '\0';
}

static kdb_unsigned_long_long_t isValidKey (Key * key)
{

	const char * value = keyString (key);
	// else we manipulate the original
	char * tempval = elektraStrDup (value);

	char * endPtr;


	// convert to long, if valid key, pointer should point to spaces or unit suffix like MB, GB
	ELEKTRA_UNSIGNED_LONG_LONG_S (tempval, &endPtr, 10);

	// before deblanking check if pointer is invalid
	if (endPtr == tempval)
	{
		elektraFree (tempval);
		return 0;
	}

	// remove possibly occurring blanks
	deblank (endPtr);
	kdb_unsigned_long_long_t factor = 0;

	// calculate the factor based on the suffix of the mameory value, return 0 if there is no matching to indicate an error of the
	// function
	if (strcmp (endPtr, "KB") == 0)
	{
		factor = 1000;
	}
	else if (strcmp (endPtr, "MB") == 0)
	{
		factor = 1000000;
	}
	else if (strcmp (endPtr, "GB") == 0)
	{
		factor = 1000000000;
	}
	else if (strcmp (endPtr, "TB") == 0)
	{
		factor = 1000000000000;
	}
	else if (strcmp (endPtr, "PB") == 0)
	{
		factor = 1000000000000000;
	}
	else if (strcmp (endPtr, "B") == 0)
	{
		factor = 1;
	}

	elektraFree (tempval);
	return factor;
}

// @param formatFactor unsigned long, used to determine the factor for normalizing to bytes
static int elektraUnitConvertToByteString (Key * key, kdb_unsigned_long_long_t formatFactor)
{

	const char * str = keyString (key);
	char * origvalue = elektraStrDup (str);
	char * ptr;
	kdb_unsigned_long_long_t ret;
	kdb_unsigned_long_long_t normalizedMemVal;

	ret = ELEKTRA_UNSIGNED_LONG_LONG_S (str, &ptr, 10);

	// check if return value within bounds
	if (ret > UINT64_MAX / formatFactor)
	{
		elektraFree (origvalue);
		return 1;
	}

	normalizedMemVal = ret * formatFactor;

	// convert back to string
	const int n = snprintf (NULL, 0, ELEKTRA_UNSIGNED_LONG_LONG_F, normalizedMemVal);
	char buf[n + 1];

	snprintf (buf, n + 1, ELEKTRA_UNSIGNED_LONG_LONG_F, normalizedMemVal);

	keySetString (key, buf);
	keySetMeta (key, "origvalue", origvalue);
	elektraFree (origvalue);
	return 0;
}

static void elektraUnitRestore (Key * key)
{
	const Key * oldval = keyGetMeta (key, "origvalue");
	if (oldval != NULL)
	{
		keySetString (key, keyString (oldval));
	}
}


int elektraUnitGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/unit"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/unit", KEY_VALUE, "unit plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/unit/exports", KEY_END),
			       keyNew ("system:/elektra/modules/unit/exports/get", KEY_FUNC, elektraUnitGet, KEY_END),
			       keyNew ("system:/elektra/modules/unit/exports/set", KEY_FUNC, elektraUnitSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/unit/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	Key * cur;
	int rc = 1;

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		cur = ksAtCursor (returned, it);
		const Key * meta = keyGetMeta (cur, "check/unit");
		if (meta)
		{
			kdb_unsigned_long_long_t format = isValidKey (cur);

			if (format == 0)
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
					parentKey,
					"The string '%s' is not following the format guidelines of (<numerical value><optional "
					"space><memory unit>, e.g. 128 MB) !",
					keyString (cur));
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			elektraUnitConvertToByteString (cur, format);
		}
	}
	return rc;
}

int elektraUnitSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
		const Key * meta = keyGetMeta (cur, "check/unit");
		if (!meta)
		{
			continue;
		}

		elektraUnitRestore (cur);
		kdb_unsigned_long_long_t format = isValidKey (cur);

		if (format == 0)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
								 "The string '%s' is not following the format guidelines of (<numerical "
								 "value><optional space><memory unit>, "
								 "e.g. 128 MB) !",
								 keyString (cur));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}


Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("unit", ELEKTRA_PLUGIN_GET, &elektraUnitGet, ELEKTRA_PLUGIN_SET, &elektraUnitSet, ELEKTRA_PLUGIN_END);
}
