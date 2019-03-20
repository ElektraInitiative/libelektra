/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "types.h"
#include "newtype.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <kdbhelper.h>

#include <elektra/conversion.h>
#include <kdberrors.h>

#define CHECK_TYPE(key, var, toValue)                                                                                                      \
	{                                                                                                                                  \
		if (strlen (keyString (key)) == 0 || toValue (key, &var) != 1)                                                             \
		{                                                                                                                          \
			return false;                                                                                                      \
		}                                                                                                                          \
	}

#define CHECK_TYPE_REVERSIBLE(key, var, toString)                                                                                          \
	{                                                                                                                                  \
		char * string = toString (var);                                                                                            \
		if (strcmp (keyString (key), string) != 0)                                                                                 \
		{                                                                                                                          \
			elektraFree (string);                                                                                              \
			return false;                                                                                                      \
		}                                                                                                                          \
		elektraFree (string);                                                                                                      \
	}

bool elektraNewTypeCheckAny (const Key * key ELEKTRA_UNUSED)
{
	return true;
}

bool elektraNewTypeCheckEmpty (const Key * key)
{
	return strlen (keyString (key)) == 0;
}

bool elektraNewTypeCheckChar (const Key * key)
{
	return strlen (keyString (key)) == 1;
}


bool elektraNewTypeCheckWChar (const Key * key)
{
	wchar_t out[2];
	return mbstowcs (out, keyString (key), 2) == 1;
}

bool elektraNewTypeCheckString (const Key * key)
{
	return strlen (keyString (key)) != 0;
}

bool elektraNewTypeCheckWString (const Key * key)
{
	const char * value = keyString (key);
	size_t max = strlen (value) + 1;
	wchar_t * wvalue = elektraCalloc (sizeof (wchar_t) * max);
	size_t result = mbstowcs (wvalue, value, max);
	elektraFree (wvalue);
	return result > 0 && result < max;
}

bool elektraNewTypeNormalizeBoolean (Plugin * handle, Key * key)
{
	const char * value = keyString (key);

	NewTypeData * data = elektraPluginGetData (handle);

	const Key * trueOverride = keyGetMeta (key, "check/boolean/true");
	const Key * falseOverride = keyGetMeta (key, "check/boolean/false");

	if ((trueOverride == NULL) != (falseOverride == NULL))
	{
		return false;
	}
	else if (trueOverride != NULL)
	{
		if (strcasecmp (keyString (trueOverride), value) == 0 || strcmp ("1", value) == 0)
		{
			keySetString (key, "1");
			keySetMeta (key, "origvalue", keyString (trueOverride));
			return true;
		}
		else if (strcasecmp (keyString (falseOverride), value) == 0 || strcmp ("0", value) == 0)
		{
			keySetString (key, "0");
			keySetMeta (key, "origvalue", keyString (falseOverride));
			return true;
		}
		return false;
	}

	if ((value[0] == '1' || value[0] == '0') && value[1] == '\0')
	{
		return true;
	}

	char * origValue = elektraStrDup (value);

	for (kdb_long_long_t i = 0; i < data->booleanCount; ++i)
	{
		if (strcasecmp (data->booleans[i].trueValue, value) == 0)
		{
			keySetString (key, "1");
			keySetMeta (key, "origvalue", origValue);
			elektraFree (origValue);
			return true;
		}
		else if (strcasecmp (data->booleans[i].falseValue, value) == 0)
		{
			keySetString (key, "0");
			keySetMeta (key, "origvalue", origValue);
			elektraFree (origValue);
			return true;
		}
	}

	elektraFree (origValue);
	return false;
}

bool elektraNewTypeCheckBoolean (const Key * key)
{
	const char * value = keyString (key);
	return (value[0] == '1' || value[0] == '0') && value[1] == '\0';
}

bool elektraNewTypeRestoreBoolean (Plugin * handle ELEKTRA_UNUSED, Key * key)
{
	const Key * orig = keyGetMeta (key, "origvalue");
	if (orig != NULL)
	{
		keySetString (key, keyString (orig));
	}

	return true;
}

bool elektraNewTypeCheckFloat (const Key * key)
{
	kdb_float_t value;
	CHECK_TYPE (key, value, elektraKeyToFloat)
	return true;
}

bool elektraNewTypeCheckDouble (const Key * key)
{
	kdb_double_t value;
	CHECK_TYPE (key, value, elektraKeyToDouble)
	return true;
}

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
bool elektraNewTypeCheckLongDouble (const Key * key)
{
	kdb_long_double_t value;
	CHECK_TYPE (key, value, elektraKeyToLongDouble)
	return true;
}

#endif

bool elektraNewTypeCheckShort (const Key * key)
{
	kdb_short_t value;
	CHECK_TYPE (key, value, elektraKeyToShort)
	CHECK_TYPE_REVERSIBLE (key, value, elektraShortToString);
	return true;
}

bool elektraNewTypeCheckLong (const Key * key)
{
	kdb_long_t value;
	CHECK_TYPE (key, value, elektraKeyToLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraLongToString);
	return true;
}

bool elektraNewTypeCheckLongLong (const Key * key)
{
	kdb_long_long_t value;
	CHECK_TYPE (key, value, elektraKeyToLongLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraLongLongToString);
	return true;
}

bool elektraNewTypeCheckUnsignedShort (const Key * key)
{
	kdb_unsigned_short_t value;
	CHECK_TYPE (key, value, elektraKeyToUnsignedShort)
	CHECK_TYPE_REVERSIBLE (key, value, elektraUnsignedShortToString);
	return true;
}

bool elektraNewTypeCheckUnsignedLong (const Key * key)
{
	kdb_unsigned_long_t value;
	CHECK_TYPE (key, value, elektraKeyToUnsignedLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraUnsignedLongToString);
	return true;
}

bool elektraNewTypeCheckUnsignedLongLong (const Key * key)
{
	kdb_unsigned_long_long_t value;
	CHECK_TYPE (key, value, elektraKeyToUnsignedLongLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraUnsignedLongLongToString);
	return true;
}

bool elektraNewTypeCheckEnum (const Key * key)
{
	const Key * multiEnum = keyGetMeta (key, "check/enum/delimiter");

	const Key * maxKey = keyGetMeta (key, "check/enum");
	const char * max = maxKey == NULL ? NULL : keyString (maxKey);

	if (max == NULL)
	{
		return false;
	}

	KeySet * validValues = ksNew (0, KS_END);
	char elem[sizeof ("check/enum/") + ELEKTRA_MAX_ARRAY_SIZE];
	strcpy (elem, "check/enum/");
	char * indexStart = elem + sizeof ("check/enum/") - 1;

	kdb_long_long_t index = 0;
	elektraWriteArrayNumber (indexStart, index);
	while (strcmp (indexStart, max) <= 0)
	{
		const Key * enumKey = keyGetMeta (key, elem);
		const char * name = enumKey != NULL ? keyString (enumKey) : "";
		if (strlen (name) > 0)
		{
			ksAppendKey (validValues, keyNew (name, KEY_META_NAME, KEY_END));
		}

		++index;
		elektraWriteArrayNumber (indexStart, index);
	}

	char delim = 0;
	if (multiEnum != NULL)
	{
		const char * delimString = keyString (multiEnum);

		if (strlen (delimString) != 1)
		{
			return false;
		}
		delim = delimString[0];
	}

	char * values = elektraStrDup (keyString (key));
	char * value = values;
	char * next;

	if (multiEnum != NULL)
	{
		while ((next = strchr (value, delim)) != NULL)
		{
			*next = '\0';
			if (ksLookupByName (validValues, value, 0) == NULL)
			{
				ksDel (validValues);
				elektraFree (values);
				return false;
			}
			value = next + 1;
		}
	}

	if (ksLookupByName (validValues, value, 0) == NULL)
	{
		ksDel (validValues);
		elektraFree (values);
		return false;
	}


	ksDel (validValues);
	elektraFree (values);

	return true;
}

void elektraNewTypeSetErrorEnum (Plugin * handle ELEKTRA_UNUSED, Key * errorKey, const Key * key)
{
	const Key * maxKey = keyGetMeta (key, "check/enum");
	const char * max = maxKey == NULL ? NULL : keyString (maxKey);

	if (max == NULL)
	{
		ELEKTRA_SET_ERRORF (52, errorKey,
				    "The type 'enum' failed to match for '%s' with string: %s\n"
				    "No values allowed! (check/enum is an empty array, or parent isn't set to last element)",
				    keyName (key), keyString (key));
		return;
	}

	char * errorMessage = elektraFormat (
		"The type 'enum' failed to match for '%s' with string: %s\n"
		"Allowed values:",
		keyName (key), keyString (key));

	char elem[sizeof ("check/enum/") + ELEKTRA_MAX_ARRAY_SIZE];
	strcpy (elem, "check/enum/");
	char * indexStart = elem + sizeof ("check/enum/") - 1;

	kdb_long_long_t index = 0;
	elektraWriteArrayNumber (indexStart, index);
	while (strcmp (indexStart, max) <= 0)
	{
		const Key * enumKey = keyGetMeta (key, elem);
		const char * name = enumKey != NULL ? keyString (enumKey) : "";
		if (strlen (name) > 0)
		{
			char * newErrorMessage = elektraFormat ("%s '%s'", errorMessage, name);
			elektraFree (errorMessage);
			errorMessage = newErrorMessage;
		}

		++index;
		elektraWriteArrayNumber (indexStart, index);
	}

	ELEKTRA_SET_ERROR (52, errorKey, errorMessage);
	elektraFree (errorMessage);
}
