/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./types.h"
#include "./type.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <elektra/core/errors.h>
#include <elektra/type/conversion.h>
#include <elektra/utility/array.h>
#include <elektra/utility/format.h>
#include <internal/macros/attributes.h>
#include <internal/utility/alloc.h>

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

bool elektraTypeCheckAny (const Key * key ELEKTRA_UNUSED)
{
	return true;
}

bool elektraTypeCheckChar (const Key * key)
{
	return strlen (keyString (key)) == 1;
}


bool elektraTypeCheckWChar (const Key * key)
{
	wchar_t out[2];
	return mbstowcs (out, keyString (key), 2) == 1;
}

bool elektraTypeCheckString (const Key * key)
{
	return keyIsString (key) == 1;
}

bool elektraTypeCheckWString (const Key * key)
{
	const char * value = keyString (key);
	size_t max = strlen (value) + 1;
	wchar_t * wvalue = elektraCalloc (sizeof (wchar_t) * max);
	size_t result = mbstowcs (wvalue, value, max);
	elektraFree (wvalue);
	return result > 0 && result < max;
}

bool elektraTypeNormalizeBoolean (Plugin * handle, Key * key)
{
	const char * value = keyString (key);

	TypeData * data = elektraPluginGetData (handle);

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

	const char * origTrueValue;
	const char * origFalseValue;

	bool restore = data->booleanRestore >= 0;

	if (value[0] == '1' && value[1] == '\0')
	{
		if (restore)
		{
			keySetMeta (key, "origvalue", data->booleans[data->booleanRestore].trueValue);
		}

		return true;
	}

	if (value[0] == '0' && value[1] == '\0')
	{
		if (restore)
		{
			keySetMeta (key, "origvalue", data->booleans[data->booleanRestore].falseValue);
		}

		return true;
	}

	char * origValue = elektraStrDup (value);

	origTrueValue = restore ? data->booleans[data->booleanRestore].trueValue : origValue;
	origFalseValue = restore ? data->booleans[data->booleanRestore].falseValue : origValue;

	for (kdb_long_long_t i = 0; i < data->booleanCount; ++i)
	{
		if (strcasecmp (data->booleans[i].trueValue, value) == 0)
		{
			keySetString (key, "1");
			if (data->booleanRestore != -2)
			{
				keySetMeta (key, "origvalue", origTrueValue);
			}
			elektraFree (origValue);
			return true;
		}
		else if (strcasecmp (data->booleans[i].falseValue, value) == 0)
		{
			keySetString (key, "0");
			if (data->booleanRestore != -2)
			{
				keySetMeta (key, "origvalue", origFalseValue);
			}
			elektraFree (origValue);
			return true;
		}
	}

	elektraFree (origValue);
	return false;
}

bool elektraTypeCheckBoolean (const Key * key)
{
	const char * value = keyString (key);
	return (value[0] == '1' || value[0] == '0') && value[1] == '\0';
}

bool elektraTypeRestoreBoolean (Plugin * handle ELEKTRA_UNUSED, Key * key)
{
	const Key * orig = keyGetMeta (key, "origvalue");
	if (orig != NULL)
	{
		keySetString (key, keyString (orig));
	}

	return true;
}

bool elektraTypeCheckFloat (const Key * key)
{
	kdb_float_t value;
	CHECK_TYPE (key, value, elektraKeyToFloat)
	return true;
}

bool elektraTypeCheckDouble (const Key * key)
{
	kdb_double_t value;
	CHECK_TYPE (key, value, elektraKeyToDouble)
	return true;
}

bool elektraTypeCheckLongDouble (const Key * key)
{
	kdb_long_double_t value;
	CHECK_TYPE (key, value, elektraKeyToLongDouble)
	return true;
}

bool elektraTypeCheckShort (const Key * key)
{
	kdb_short_t value;
	CHECK_TYPE (key, value, elektraKeyToShort)
	CHECK_TYPE_REVERSIBLE (key, value, elektraShortToString);
	return true;
}

bool elektraTypeCheckLong (const Key * key)
{
	kdb_long_t value;
	CHECK_TYPE (key, value, elektraKeyToLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraLongToString);
	return true;
}

bool elektraTypeCheckLongLong (const Key * key)
{
	kdb_long_long_t value;
	CHECK_TYPE (key, value, elektraKeyToLongLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraLongLongToString);
	return true;
}

bool elektraTypeCheckUnsignedShort (const Key * key)
{
	kdb_unsigned_short_t value;
	CHECK_TYPE (key, value, elektraKeyToUnsignedShort)
	CHECK_TYPE_REVERSIBLE (key, value, elektraUnsignedShortToString);
	return true;
}

bool elektraTypeCheckUnsignedLong (const Key * key)
{
	kdb_unsigned_long_t value;
	CHECK_TYPE (key, value, elektraKeyToUnsignedLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraUnsignedLongToString);
	return true;
}

bool elektraTypeCheckUnsignedLongLong (const Key * key)
{
	kdb_unsigned_long_long_t value;
	CHECK_TYPE (key, value, elektraKeyToUnsignedLongLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraUnsignedLongLongToString);
	return true;
}

static bool enumValidValues (const Key * key, KeySet * validValues, char * delim)
{

	const Key * maxKey = keyGetMeta (key, "check/enum");
	const char * max = maxKey == NULL ? NULL : keyString (maxKey);

	if (max == NULL)
	{
		return false;
	}

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
			kdb_unsigned_long_long_t val = index;
			Key * k = keyNew ("user:/", KEY_BINARY, KEY_SIZE, sizeof (kdb_unsigned_long_long_t), KEY_VALUE, &val, KEY_END);
			keyAddName (k, name);
			ksAppendKey (validValues, k);
		}

		++index;
		elektraWriteArrayNumber (indexStart, index);
	}

	const Key * multiEnum = keyGetMeta (key, "check/enum/delimiter");
	if (multiEnum != NULL)
	{
		const char * delimString = keyString (multiEnum);

		if (strlen (delimString) != 1)
		{
			ksDel (validValues);
			return false;
		}
		*delim = delimString[0];
	}

	return true;
}

static char * calculateStringValue (KeySet * validValues, char delimiter, kdb_unsigned_long_long_t value)
{
	char * stringValue = elektraStrDup ("");
	Key * cur = NULL;

	for (elektraCursor it = 0; it < ksGetSize (validValues); ++it)
	{
		cur = ksAtCursor (validValues, it);
		const kdb_unsigned_long_long_t * val = keyValue (cur);
		const char * name = keyName (cur) + sizeof ("user:/") - 1;
		if (delimiter == 0 && *val == value)
		{
			elektraFree (stringValue);
			return elektraStrDup (name);
		}
		else if (delimiter != 0)
		{
			if (*val == 0 && value == 0 && stringValue[0] == '\0')
			{
				elektraFree (stringValue);
				return elektraStrDup (name);
			}
			else if (*val != 0 && (*val & value) == *val)
			{
				char * tmp = stringValue[0] == '\0' ? elektraFormat ("%s", name) :
								      elektraFormat ("%s%c%s", stringValue, delimiter, name);
				elektraFree (stringValue);
				stringValue = tmp;

				value &= ~*val;
			}
		}
	}

	return stringValue;
}

bool elektraTypeNormalizeEnum (Plugin * handle ELEKTRA_UNUSED, Key * key)
{
	const Key * normalize = keyGetMeta (key, "check/enum/normalize");
	if (normalize == NULL || strcmp (keyString (normalize), "1") != 0)
	{
		return true;
	}

	KeySet * validValues = ksNew (0, KS_END);
	char delim = 0;
	if (!enumValidValues (key, validValues, &delim))
	{
		return false;
	}

	char * values = elektraStrDup (keyString (key));
	char * value = values;
	char * next;

	if (isdigit (values[0]))
	{
		kdb_unsigned_long_long_t val = ELEKTRA_UNSIGNED_LONG_LONG_S (values, NULL, 10);
		char * origValue = calculateStringValue (validValues, delim, val);
		if (origValue == NULL)
		{
			ksDel (validValues);
			elektraFree (values);
			return false;
		}

		keySetMeta (key, "origvalue", origValue);

		elektraFree (origValue);
		ksDel (validValues);
		elektraFree (values);
		return true;
	}

	Key * valueKey = keyNew ("user:/0", KEY_END);

	kdb_unsigned_long_long_t normalized = 0;
	if (delim != 0)
	{
		while ((next = strchr (value, delim)) != NULL)
		{
			*next = '\0';
			keySetBaseName (valueKey, value);
			Key * cur = ksLookup (validValues, valueKey, 0);
			if (cur == NULL)
			{
				keyDel (valueKey);
				ksDel (validValues);
				elektraFree (values);
				return false;
			}

			const kdb_unsigned_long_long_t * val = keyValue (cur);
			normalized |= *val;
			value = next + 1;
		}
	}

	keySetBaseName (valueKey, value);
	Key * cur = ksLookup (validValues, valueKey, 0);
	keyDel (valueKey);
	if (cur == NULL)
	{
		ksDel (validValues);
		elektraFree (values);
		return false;
	}

	const kdb_unsigned_long_long_t * val = keyValue (cur);
	normalized |= *val;

	ksDel (validValues);
	elektraFree (values);

	char * origValue = elektraStrDup (keyString (key));
	char * normValue = elektraFormat (ELEKTRA_UNSIGNED_LONG_LONG_F, normalized);

	keySetString (key, normValue);
	keySetMeta (key, "origvalue", origValue);

	elektraFree (origValue);
	elektraFree (normValue);

	return true;
}

bool elektraTypeCheckEnum (const Key * key)
{
	const Key * normalize = keyGetMeta (key, "check/enum/normalize");
	if (normalize != NULL && strcmp (keyString (normalize), "1") == 0)
	{
		// was already implicitly checked during normalization
		return true;
	}

	const Key * maxKey = keyGetMeta (key, "check/enum");
	const char * max = maxKey == NULL ? NULL : keyString (maxKey);

	if (max == NULL)
	{
		return false;
	}

	KeySet * validValues = ksNew (0, KS_END);
	char delim = 0;
	if (!enumValidValues (key, validValues, &delim))
	{
		return false;
	}

	char * values = elektraStrDup (keyString (key));
	char * value = values;
	char * next;

	Key * valueKey = keyNew ("user:/0", KEY_END);

	if (delim != 0)
	{
		while ((next = strchr (value, delim)) != NULL)
		{
			*next = '\0';
			keySetBaseName (valueKey, value);
			if (ksLookup (validValues, valueKey, 0) == NULL)
			{
				keyDel (valueKey);
				ksDel (validValues);
				elektraFree (values);
				return false;
			}
			value = next + 1;
		}
	}

	keySetBaseName (valueKey, value);
	if (ksLookup (validValues, valueKey, 0) == NULL)
	{
		keyDel (valueKey);
		ksDel (validValues);
		elektraFree (values);
		return false;
	}


	keyDel (valueKey);
	ksDel (validValues);
	elektraFree (values);

	return true;
}

bool elektraTypeRestoreEnum (Plugin * handle ELEKTRA_UNUSED, Key * key)
{
	const Key * orig = keyGetMeta (key, "origvalue");
	if (orig != NULL)
	{
		keySetString (key, keyString (orig));
	}

	return true;
}

void elektraTypeSetErrorEnum (Plugin * handle ELEKTRA_UNUSED, Key * errorKey, const Key * key)
{
	const Key * maxKey = keyGetMeta (key, "check/enum");
	const char * max = maxKey == NULL ? NULL : keyString (maxKey);

	if (max == NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
			errorKey,
			"The type 'enum' failed to match for '%s' with string: '%s'\n"
			"No values are allowed (check/enum is an empty array, or parent isn't set to last element)",
			keyName (key), keyString (key));
		return;
	}

	char * errorMessage = elektraFormat (
		"The type 'enum' failed to match for '%s' with string: '%s'\n"
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

	ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, errorMessage);
	elektraFree (errorMessage);
}
