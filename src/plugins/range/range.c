/**
 * @file
 *
 * @brief Source for range plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "range.h"
#include <ctype.h>
#include <errno.h>
#include <internal/utility/assert.h>
#include <elektra/kdb/errors.h>
#include <internal/utility/old_helper.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>


typedef enum
{
	INT,
	UINT,
	FLOAT,
	CHAR,
	HEX,
	NA,
} RangeType;

typedef struct
{
	RangeType type;
	union uValue
	{
		unsigned long long int i;
		long double f;
	} Value;
} RangeValue;


// switch min and max values if needed and apply -1 factor
static void normalizeValues (RangeType type, RangeValue * min, RangeValue * max, RangeValue * a, RangeValue * b, int factorA, int factorB)
{
	unsigned long long int tmpIA = factorA == -1 ? ULLONG_MAX - (*a).Value.i + 1 : (*a).Value.i;
	unsigned long long int tmpIB = factorB == -1 ? ULLONG_MAX - (*b).Value.i + 1 : (*b).Value.i;
	long double tmpFA = factorA * (*a).Value.f;
	long double tmpFB = factorB * (*b).Value.f;
	switch (type)
	{
	case INT:
	case HEX:
	case CHAR:
		if ((long long) tmpIA <= (long long) tmpIB)
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
	case UINT:
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
}


// parse value starting at ptr and set ptr to the first character
// after value. return a RangeValue with type == NA on error

RangeValue strToValue (const char ** ptr, RangeType type)
{
	RangeValue v;
	v.type = type;
	v.Value.i = 0;
	char * endPtr = NULL;

	errno = 0; // the c std library doesn't reset errno, so do it before conversions to be safe
	switch (type)
	{
	case INT:
	case UINT:
		v.Value.i = strtoull (*ptr, &endPtr, 10);
		if (errno == ERANGE || (errno != 0 && v.Value.i == 0))
		{
			v.type = NA;
		}
		break;
	case FLOAT:
		v.Value.f = strtold (*ptr, &endPtr);
		if (errno == ERANGE || (errno != 0 && fpclassify (v.Value.f) == FP_ZERO))
		{
			v.type = NA;
		}
		break;
	case HEX:
		v.Value.i = strtoull (*ptr, &endPtr, 16);
		if (errno == ERANGE || (errno != 0 && v.Value.i == 0))
		{
			v.type = NA;
		}
		break;
	case CHAR:
		if (!isalpha (**ptr))
		{
			v.type = NA;
		}
		v.Value.i = **ptr;
		endPtr = (char *) *ptr + 1;
	default:
		break;
	}
	*ptr = endPtr;
	return v;
}

// parse string into min - max values
// return -1 on error, 0 on success;

static int rangeStringToRange (const char * rangeString, RangeValue * min, RangeValue * max, RangeType type)
{
	int factorA = 1; // multiplication factors for parsed values
	int factorB = 1; // if '-' is read, factor will be set to -1
	RangeValue a, b;
	a.type = type;
	b.type = type;
	a.Value.i = 0;
	b.Value.i = 0;
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
				if (type == UINT)
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
				if (type == UINT)
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
				a = strToValue (&ptr, type);
			}
			else if (pos == 2)
			{
				pos = 3;
				b = strToValue (&ptr, type);
				if (b.type == NA)
				{
					return -1;
				}
			}
			else
			{
				return -1;
			}
		}
		else
		{
			return -1;
		}
	}
	if (pos != 1 && pos != 3)
	{
		return -1;
	}
	if (pos == 1)
	{
		b = a;
		factorB = factorA;
	}
	normalizeValues (type, min, max, &a, &b, factorA, factorB);
	return 0;
}


static int validateSingleRange (const char * valueStr, const char * rangeString, RangeType type)
{
	RangeValue min, max;
	min.Value.i = 0;
	max.Value.i = 0;
	min.type = type;
	max.type = type;
	int rc = rangeStringToRange (rangeString, &min, &max, type);
	if (rc)
	{
		return -1;
	}
	RangeValue val;
	val.type = type;
	val.Value.i = 0;
	char * endPtr;
	errno = 0; // the c std library doesn't reset errno, so do it before conversions to be safe
	switch (type)
	{
	case INT:
	case UINT:
		val.Value.i = strtoull (valueStr, &endPtr, 10);
		break;
	case FLOAT:
		val.Value.f = strtold (valueStr, &endPtr);
		break;
	case HEX:
		val.Value.i = strtoull (valueStr, &endPtr, 16);
		break;
	case CHAR:
		val.Value.i = valueStr[0];
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
	case CHAR:
		if ((long long) val.Value.i < (long long) min.Value.i || (long long) val.Value.i > (long long) max.Value.i)
		{
			return 0;
		}
		else
		{
			return 1;
		}
		break;
	case UINT:
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
		return -1;
		break;
	}
}

static int validateMultipleRanges (const char * valueStr, const char * rangeString, Key * parentKey, RangeType type)
{
	char * localCopy = elektraStrDup (rangeString);
	char * savePtr = NULL;
	char * token = strtok_r (localCopy, ",", &savePtr);
	int rc = validateSingleRange (valueStr, token, type);
	if (rc == 1)
	{
		elektraFree (localCopy);
		return 1;
	}
	else if (rc == -1)
	{
		elektraFree (localCopy);
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Invalid syntax: %s", token);
		return -1;
	}
	while ((token = strtok_r (NULL, ",", &savePtr)) != NULL)
	{
		rc = validateSingleRange (valueStr, token, type);
		if (rc == 1)
		{
			elektraFree (localCopy);
			return 1;
		}
		else if (rc == -1)
		{
			elektraFree (localCopy);
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Invalid syntax: %s", token);
			return -1;
		}
	}
	elektraFree (localCopy);
	return 0;
}

static RangeType stringToType (const Key * typeMeta)
{
	if (typeMeta)
	{
		static const char * intTypes[] = {
			"short",
			"long",
			"long long",
			NULL,
		};
		static const char * uintTypes[] = {
			"unsigned short",
			"unsigned long",
			"unsigned long long",
			NULL,
		};
		static const char * floatTypes[] = {
			"float",
			"double",
			"long double",
			NULL,
		};
		const char * strVal = keyString (typeMeta);
		for (int i = 0; intTypes[i] != NULL; ++i)
		{
			if (!strcasecmp (strVal, intTypes[i])) return INT;
		}
		for (int i = 0; uintTypes[i] != NULL; ++i)
		{
			if (!strcasecmp (strVal, uintTypes[i])) return UINT;
		}
		for (int i = 0; floatTypes[i] != NULL; ++i)
		{
			if (!strcasecmp (strVal, floatTypes[i])) return FLOAT;
		}
		if (!strcasecmp (strVal, "char"))
			return CHAR;
		else if (!strcasecmp (strVal, "HEX"))
			return HEX;
	}
	return NA;
}

static RangeType getType (const Key * key)
{
	const Key * typeMeta = keyGetMeta (key, "check/type");

	// If "check/type" is not specified, fall back to "type".
	// As decided in https://github.com/ElektraInitiative/libelektra/issues/3984#issuecomment-909144492
	if (typeMeta == NULL)
	{
		typeMeta = keyGetMeta (key, "type");
	}

	RangeType type = stringToType (typeMeta);

	if (type == NA)
		return INT;
	else
		return type;
}

static int validateKey (Key * key, Key * parentKey, bool errorsAsWarnings)
{
	const Key * rangeMeta = keyGetMeta (key, "check/range");
	const char * rangeString = keyString (rangeMeta);
	RangeType type = getType (key);
	if (type == UINT)
	{
		const char * ptr = keyString (key);
		while (*ptr)
		{
			if (*ptr == '-')
			{
				return -1;
			}
			else if (isdigit (*ptr))
			{
				break;
			}
			++ptr;
		}
	}

	if (!strchr (rangeString, ','))
	{
		int rc = validateSingleRange (keyString (key), rangeString, type);
		if (rc == -1)
		{
			if (errorsAsWarnings)
			{
				ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (parentKey, "Invalid syntax: %s", keyString (rangeMeta));
				return 1;
			}
			else
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Invalid syntax: %s", keyString (rangeMeta));
				return -1;
			}
		}
		else if (rc == 0)
		{
			if (errorsAsWarnings)
			{
				ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (parentKey, RANGE_ERROR_MESSAGE, keyString (key), keyName (key),
									  rangeString);
				return 0;
			}
			else
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, RANGE_ERROR_MESSAGE, keyString (key), keyName (key),
									rangeString);
				return 0;
			}
		}
		else
		{
			return rc;
		}
	}
	else
	{
		int rc = validateMultipleRanges (keyString (key), rangeString, parentKey, type);
		if (rc == 0)
		{
			if (errorsAsWarnings)
			{
				ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (parentKey, RANGE_ERROR_MESSAGE, keyString (key), keyName (key),
									  rangeString);
			}
			else
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, RANGE_ERROR_MESSAGE, keyString (key), keyName (key),
									rangeString);
			}
		}
		return rc;
	}
}

int elektraRangeGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/range"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/range", KEY_VALUE, "range plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/range/exports", KEY_END),
			       keyNew ("system:/elektra/modules/range/exports/get", KEY_FUNC, elektraRangeGet, KEY_END),
			       keyNew ("system:/elektra/modules/range/exports/set", KEY_FUNC, elektraRangeSet, KEY_END),
			       keyNew ("system:/elektra/modules/range/exports/validateKey", KEY_FUNC, validateKey, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/range/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	// Validate all keys, treat errors as warnings.
	Key * cur;

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		cur = ksAtCursor (returned, it);

		/* skip parent namespaces that differ from the key namespace,
		 * otherwise every warning would get issued multiple times */
		if (keyGetNamespace (parentKey) != keyGetNamespace (cur)) continue;

		const Key * meta = keyGetMeta (cur, "check/range");
		if (meta)
		{
			validateKey (cur, parentKey, true);
		}
	}

	// Always return 1. We don't want kdbGet() to fail because of validation problems.
	return ELEKTRA_PLUGIN_STATUS_SUCCESS; // success
}

int elektraRangeSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	Key * cur;

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		cur = ksAtCursor (returned, it);
		const Key * meta = keyGetMeta (cur, "check/range");
		if (meta)
		{
			int rc = validateKey (cur, parentKey, false);
			if (rc <= 0)
			{
				return -1;
			}
		}
	}
	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
    return elektraPluginExport ("range",
	    ELEKTRA_PLUGIN_GET,	&elektraRangeGet,
	    ELEKTRA_PLUGIN_SET,	&elektraRangeSet,
	    ELEKTRA_PLUGIN_END);
}

