/**
 * @file
 *
 * @brief Source for range plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "range.h"
#include <ctype.h>
#include <errno.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int elektraRangeOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return 1; // success
}

int elektraRangeClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return 1; // success
}

static int rangeStringToRange (const char * rangeString, long long int * min, long long int * max)
{
	int factorA = 1;
	int factorB = 1;
	long long int a, b;
	a = b = 0;
	int pos = 0;

	const char * ptr = rangeString;
	while (*ptr)
	{
		if (isspace (*ptr))
		{
			++ptr;
			continue;
		}
		else if (*ptr == '-')
		{
			if (pos == 0)
			{
				if (factorA == -1)
				{
					return -1;
				}
				factorA = -1;
			}
			else if (pos == 1)
			{
				pos = 2;
			}
			else if (pos == 2)
			{
				if (factorB == -1)
				{
					return -1;
				}
				factorB = -1;
			}
			else
			{
				return -1;
			}
			++ptr;
			continue;
		}
		else if (isdigit (*ptr))
		{
			if (pos == 0)
			{
				pos = 1;
				char * endPtr;
				a = strtoll (ptr, &endPtr, 10);
				ptr = endPtr;
				if (errno == ERANGE || (errno != 0 && a == 0))
				{
					return -1;
				}
			}
			else if (pos == 2)
			{
				pos = 3;
				char * endPtr;
				b = strtoll (ptr, &endPtr, 10);
				ptr = endPtr;
				if (errno == ERANGE || (errno != 0 && b == 0))
				{
					return -1;
				}
			}
		}
		else
		{
			return -1;
		}
	}
	if (pos != 3)
	{
		return -1;
	}
	long long int tmpA = factorA * a;
	long long int tmpB = factorB * b;
	if (tmpA <= tmpB)
	{
		*min = tmpA;
		*max = tmpB;
	}
	else
	{
		*min = tmpB;
		*max = tmpA;
	}
	return 0;
}

static int validateSingleRange (const char * valueStr, const char * rangeString, Key * parentKey)
{
	long long int min, max;
	min = max = 0;
	int rc = rangeStringToRange (rangeString, &min, &max);
	fprintf (stderr, "%s: ret: %d, min: %lld, max: %lld\n", rangeString, rc, min, max);
	if (rc)
	{
		return -1;
	}
	long long int val = 0;
	char * endPtr;
	val = strtoll (valueStr, &endPtr, 10);
	if (errno == ERANGE || (errno != 0 && val == 0))
	{
		return -1;
	}
	if (val < min || val > max)
	{
		return 0;
	}
	else
	{
		return 1;
	}
}

static int validateMultipleRanges (const char * valueStr, const char * rangeString, Key * parentKey)
{
	char * localCopy = elektraStrDup (rangeString);
	char * savePtr = NULL;
	;
	char * token = NULL;
	;
	token = strtok_r (localCopy, ",", &savePtr);
	int rc = validateSingleRange (valueStr, token, parentKey);
	if (rc == 1)
	{
		elektraFree (localCopy);
		return 1;
	}
	while ((token = strtok_r (NULL, ",", &savePtr)) != NULL)
	{
		rc = validateSingleRange (valueStr, token, parentKey);
		if (rc == 1)
		{
			elektraFree (localCopy);
			return 1;
		}
	}
	elektraFree (localCopy);
	return 0;
}

static int validateKey (Key * key, Key * parentKey)
{
	const Key * rangeMeta = keyGetMeta (key, "check/range");
	const char * rangeString = keyString (rangeMeta);
	if (!strchr (rangeString, ','))
	{
		return validateSingleRange (keyString (key), rangeString, parentKey);
	}
	else
	{
		return validateMultipleRanges (keyString (key), rangeString, parentKey);
	}
}

int elektraRangeGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/range"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/range", KEY_VALUE, "range plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/range/exports", KEY_END),
			       keyNew ("system/elektra/modules/range/exports/open", KEY_FUNC, elektraRangeOpen, KEY_END),
			       keyNew ("system/elektra/modules/range/exports/close", KEY_FUNC, elektraRangeClose, KEY_END),
			       keyNew ("system/elektra/modules/range/exports/get", KEY_FUNC, elektraRangeGet, KEY_END),
			       keyNew ("system/elektra/modules/range/exports/set", KEY_FUNC, elektraRangeSet, KEY_END),
			       keyNew ("system/elektra/modules/range/exports/error", KEY_FUNC, elektraRangeError, KEY_END),
			       keyNew ("system/elektra/modules/range/exports/checkconf", KEY_FUNC, elektraRangeCheckConfig, KEY_END),
#include ELEKTRA_README (range)
			       keyNew ("system/elektra/modules/range/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys

	return 1; // success
}

int elektraRangeSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	Key * cur;
	while ((cur = ksNext (returned)) != NULL)
	{
		if (keyGetMeta (cur, "check/range"))
		{
			int rc = validateKey (cur, parentKey);
			if (rc < 1)
			{
				return -1;
			}
		}
	}
	return 1; // success
}

int elektraRangeError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return 1; // success
}

int elektraRangeCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (range)
{
	// clang-format off
    return elektraPluginExport ("range",
	    ELEKTRA_PLUGIN_OPEN,	&elektraRangeOpen,
	    ELEKTRA_PLUGIN_CLOSE,	&elektraRangeClose,
	    ELEKTRA_PLUGIN_GET,	&elektraRangeGet,
	    ELEKTRA_PLUGIN_SET,	&elektraRangeSet,
	    ELEKTRA_PLUGIN_ERROR,	&elektraRangeError,
	    ELEKTRA_PLUGIN_END);
}

