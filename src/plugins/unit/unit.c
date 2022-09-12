/**
 * @file
 *
 * @brief unit plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "unit.h"
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbtypes.h>
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

static kdb_unsigned_long_long_t isValidKey (ElektraKey * key)
{

	const char * value = elektraKeyString (key);
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
static int elektraUnitConvertToByteString (ElektraKey * key, kdb_unsigned_long_long_t formatFactor)
{

	const char * str = elektraKeyString (key);
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

	elektraKeySetString (key, buf);
	elektraKeySetMeta (key, "origvalue", origvalue);
	elektraFree (origvalue);
	return 0;
}

static void elektraUnitRestore (ElektraKey * key)
{
	const ElektraKey * oldval = elektraKeyGetMeta (key, "origvalue");
	if (oldval != NULL)
	{
		elektraKeySetString (key, elektraKeyString (oldval));
	}
}


int elektraUnitGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/unit"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/unit", ELEKTRA_KEY_VALUE, "unit plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/unit/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/unit/exports/get", ELEKTRA_KEY_FUNC, elektraUnitGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/unit/exports/set", ELEKTRA_KEY_FUNC, elektraUnitSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/unit/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	ElektraKey * cur;
	int rc = 1;
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		const ElektraKey * meta = elektraKeyGetMeta (cur, "check/unit");
		if (meta)
		{
			kdb_unsigned_long_long_t format = isValidKey (cur);

			if (format == 0)
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
					parentKey,
					"The string '%s' is not following the format guidelines of (<numerical value><optional "
					"space><memory unit>, e.g. 128 MB) !",
					elektraKeyString (cur));
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			elektraUnitConvertToByteString (cur, format);
		}
	}
	return rc;
}

int elektraUnitSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{

	ElektraKey * cur;
	elektraKeysetRewind (returned);
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		const ElektraKey * meta = elektraKeyGetMeta (cur, "check/unit");
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
								 elektraKeyString (cur));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}


Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("unit", ELEKTRA_PLUGIN_GET, &elektraUnitGet, ELEKTRA_PLUGIN_SET, &elektraUnitSet, ELEKTRA_PLUGIN_END);
}
