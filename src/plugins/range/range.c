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

typedef enum {
	INT,
	FLOAT,
	CHAR,
	HEX,
} RangeType;

typedef struct
{
	RangeType type;
	union _Value {
		long long int i;
		long double f;
	} Value;
} RangeValue;

static int rangeStringToRange (const char * rangeString, RangeValue * min, RangeValue * max, RangeType type)
{
	int factorA = 1;
	int factorB = 1;
	long long int ia, ib;
	ia = ib = 0;
	long double fa, fb;
	fa = fb = 0;
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
		else if (isalnum (*ptr))
		{
			if (pos == 0)
			{
				pos = 1;
				char * endPtr;
				switch (type)
				{
				case INT:
					ia = strtoll (ptr, &endPtr, 10);
					if (errno == ERANGE || (errno != 0 && ia == 0))
					{
						return -1;
					}
					break;
				case FLOAT:
					fa = strtold (ptr, &endPtr);
					if (errno == ERANGE || (errno != 0 && fa == 0))
					{
						return -1;
					}
					break;
				case HEX:
					ia = strtoll (ptr, &endPtr, 16);
					if (errno == ERANGE || (errno != 0 && ia == 0))
					{
						return -1;
					}
					break;
				case CHAR:

					break;
				default:
					break;
				}
				ptr = endPtr;
			}
			else if (pos == 2)
			{
				pos = 3;
				char * endPtr;
				switch (type)
				{
				case INT:
					ib = strtoll (ptr, &endPtr, 10);
					if (errno == ERANGE || (errno != 0 && ib == 0))
					{
						return -1;
					}
					break;
				case FLOAT:
					fb = strtold (ptr, &endPtr);
					if (errno == ERANGE || (errno != 0 && fb == 0))
					{
						return -1;
					}
					break;
				case HEX:
					ib = strtoll (ptr, &endPtr, 16);
					if (errno == ERANGE || (errno != 0 && ib == 0))
					{
						return -1;
					}
					break;

				default:
					break;
				}
				ptr = endPtr;
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
	long long int tmpIA = factorA * ia;
	long long int tmpIB = factorB * ib;
	long double tmpFA = factorA * fa;
	long double tmpFB = factorB * fb;
	switch (type)
	{
	case INT:
	case HEX:
		if (tmpIA <= tmpIB)
		{
			min->Value.i = tmpIA;
			max->Value.i = tmpIB;
		}
		else
		{
			min->Value.i = tmpIB;
			max->Value.i = tmpIA;
		}
		break;
	case FLOAT:
		if (tmpFA <= tmpFB)
		{
			min->Value.f = tmpFA;
			max->Value.f = tmpFB;
		}
		else
		{
			min->Value.f = tmpFB;
			max->Value.f = tmpFA;
		}
		break;
	default:
		break;
	}
	return 0;
}

static int validateSingleRange (const char * valueStr, const char * rangeString, Key * parentKey, RangeType type)
{
	RangeValue min, max;
	min.Value.i = 0;
	max.Value.i = 0;
	min.type = type;
	max.type = type;
	int rc = rangeStringToRange (rangeString, &min, &max, type);
	switch (type)
	{
	case INT:
		fprintf (stderr, "%s: ret: %d, min: %lld, max: %lld\n", rangeString, rc, min.Value.i, max.Value.i);
		break;
	case FLOAT:
		fprintf (stderr, "%s: ret: %d, min: %Lf, max: %Lf\n", rangeString, rc, min.Value.f, max.Value.f);
		break;
	case HEX:
		fprintf (stderr, "%s: ret: %d, min: 0x%x, max: 0x%x\n", rangeString, rc, (unsigned int)min.Value.i,
			 (unsigned int)max.Value.i);
		break;
	}
	if (rc)
	{
		return -1;
	}
	RangeValue val;
	val.type = type;
	val.Value.i = 0;
	char * endPtr;
	switch (type)
	{
	case INT:
		val.Value.i = strtoll (valueStr, &endPtr, 10);
		break;
	case FLOAT:
		val.Value.f = strtold (valueStr, &endPtr);
		break;
	case HEX:
		val.Value.i = strtoll (valueStr, &endPtr, 16);
		break;
	default:
		break;
	}
	if (errno == ERANGE || (errno != 0 && val.Value.i == 0))
	{
		return -1;
	}
	switch (type)
	{
	case INT:
	case HEX:
		if (val.Value.i < min.Value.i || val.Value.i > max.Value.i)
		{
			return 0;
		}
		else
		{
			return 1;
		}
		break;
	case FLOAT:
		if (val.Value.f < min.Value.f || val.Value.f > max.Value.f)
		{
			return 0;
		}
		else
		{
			return 1;
		}
		break;
	default:
		break;
	}
}

static int validateMultipleRanges (const char * valueStr, const char * rangeString, Key * parentKey, RangeType type)
{
	char * localCopy = elektraStrDup (rangeString);
	char * savePtr = NULL;
	char * token = NULL;
	token = strtok_r (localCopy, ",", &savePtr);
	int rc = validateSingleRange (valueStr, token, parentKey, type);
	if (rc == 1)
	{
		elektraFree (localCopy);
		return 1;
	}
	while ((token = strtok_r (NULL, ",", &savePtr)) != NULL)
	{
		rc = validateSingleRange (valueStr, token, parentKey, type);
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
	const Key * typeMeta = keyGetMeta (key, "check/range/type");
	RangeType type = INT;
	if (typeMeta)
	{
		const char * strVal = keyString (typeMeta);
		if (!strcasecmp (strVal, "INT"))
			type = INT;
		else if (!strcasecmp (strVal, "FLOAT"))
			type = FLOAT;
		else if (!strcasecmp (strVal, "CHAR"))
			type = CHAR;
		else if (!strcasecmp (strVal, "HEX"))
			type = HEX;
	}

	if (!strchr (rangeString, ','))
	{
		return validateSingleRange (keyString (key), rangeString, parentKey, type);
	}
	else
	{
		return validateMultipleRanges (keyString (key), rangeString, parentKey, type);
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

