/**
 * @file
 *
 * @brief Only a destructor
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "types.h"

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

#define DEFAULT_BOOLEANS                                                                                                                   \
	ksNew (11, keyNew ("user/booleans", KEY_VALUE, "#4", KEY_END), keyNew ("user/booleans/#0/true", KEY_VALUE, "yes", KEY_END),        \
	       keyNew ("user/booleans/#0/false", KEY_VALUE, "no", KEY_END), keyNew ("user/booleans/#1/true", KEY_VALUE, "true", KEY_END),  \
	       keyNew ("user/booleans/#1/false", KEY_VALUE, "false", KEY_END), keyNew ("user/booleans/#2/true", KEY_VALUE, "on", KEY_END), \
	       keyNew ("user/booleans/#2/false", KEY_VALUE, "off", KEY_END),                                                               \
	       keyNew ("user/booleans/#3/true", KEY_VALUE, "enabled", KEY_END),                                                            \
	       keyNew ("user/booleans/#3/false", KEY_VALUE, "disabled", KEY_END),                                                          \
	       keyNew ("user/booleans/#4/true", KEY_VALUE, "enable", KEY_END),                                                             \
	       keyNew ("user/booleans/#4/false", KEY_VALUE, "disable", KEY_END), KS_END)

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

	if ((value[0] == '1' || value[0] == '0') && value[1] == '\0')
	{
		return true;
	}

	KeySet * config = ksDup (elektraPluginGetConfig (handle));

	Key * parent = ksLookupByName (config, "/booleans", 0);
	const char * max = keyString (parent);
	if (parent == NULL || strlen (max) == 0)
	{
		ksDel (config);
		config = DEFAULT_BOOLEANS;
		parent = ksLookupByName (config, "/booleans", 0);
		max = keyString (parent);
	}

	kdb_long_long_t index = 0;
	char buffer[10 + ELEKTRA_MAX_ARRAY_SIZE + 6];
	strcpy (buffer, "/booleans/");
	char * indexPos = &buffer[10];
	elektraWriteArrayNumber (indexPos, index);

	char * origValue = elektraStrDup (value);

	while (strcmp (indexPos, max) <= 0)
	{
		char * subPos = &buffer[strlen (buffer)];
		strcpy (subPos, "/true");
		Key * trueKey = ksLookupByName (config, buffer, 0);
		strcpy (subPos, "/false");
		Key * falseKey = ksLookupByName (config, buffer, 0);

		const char * trueValue = trueKey == NULL ? "1" : keyString (trueKey);
		const char * falseValue = falseKey == NULL ? "0" : keyString (falseKey);

		if (strcasecmp (trueValue, value) == 0)
		{
			keySetString (key, "1");
			keySetMeta (key, "origvalue", origValue);
			elektraFree (origValue);
			ksDel (config);
			return true;
		}
		else if (strcasecmp (falseValue, value) == 0)
		{
			keySetString (key, "0");
			keySetMeta (key, "origvalue", origValue);
			elektraFree (origValue);
			ksDel (config);
			return true;
		}

		++index;
		elektraWriteArrayNumber (indexPos, index);
	}

	elektraFree (origValue);
	ksDel (config);
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
	const Key * multiEnum = keyGetMeta (key, "check/enum/multi");

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
