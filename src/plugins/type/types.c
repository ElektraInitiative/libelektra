/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "types.h"
#include "type.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <kdbhelper.h>

#include <kdbease.h>
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

bool elektraTypeCheckAny (const ElektraKey * key ELEKTRA_UNUSED)
{
	return true;
}

bool elektraTypeCheckChar (const ElektraKey * key)
{
	return strlen (elektraKeyString (key)) == 1;
}


bool elektraTypeCheckWChar (const ElektraKey * key)
{
	wchar_t out[2];
	return mbstowcs (out, elektraKeyString (key), 2) == 1;
}

bool elektraTypeCheckString (const ElektraKey * key)
{
	return elektraKeyIsString (key) == 1;
}

bool elektraTypeCheckWString (const ElektraKey * key)
{
	const char * value = elektraKeyString (key);
	size_t max = strlen (value) + 1;
	wchar_t * wvalue = elektraCalloc (sizeof (wchar_t) * max);
	size_t result = mbstowcs (wvalue, value, max);
	elektraFree (wvalue);
	return result > 0 && result < max;
}

bool elektraTypeNormalizeBoolean (Plugin * handle, ElektraKey * key)
{
	const char * value = elektraKeyString (key);

	TypeData * data = elektraPluginGetData (handle);

	const ElektraKey * trueOverride = elektraKeyGetMeta (key, "check/boolean/true");
	const ElektraKey * falseOverride = elektraKeyGetMeta (key, "check/boolean/false");

	if ((trueOverride == NULL) != (falseOverride == NULL))
	{
		return false;
	}
	else if (trueOverride != NULL)
	{
		if (strcasecmp (elektraKeyString (trueOverride), value) == 0 || strcmp ("1", value) == 0)
		{
			elektraKeySetString (key, "1");
			elektraKeySetMeta (key, "origvalue", elektraKeyString (trueOverride));
			return true;
		}
		else if (strcasecmp (elektraKeyString (falseOverride), value) == 0 || strcmp ("0", value) == 0)
		{
			elektraKeySetString (key, "0");
			elektraKeySetMeta (key, "origvalue", elektraKeyString (falseOverride));
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
			elektraKeySetMeta (key, "origvalue", data->booleans[data->booleanRestore].trueValue);
		}

		return true;
	}

	if (value[0] == '0' && value[1] == '\0')
	{
		if (restore)
		{
			elektraKeySetMeta (key, "origvalue", data->booleans[data->booleanRestore].falseValue);
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
			elektraKeySetString (key, "1");
			if (data->booleanRestore != -2)
			{
				elektraKeySetMeta (key, "origvalue", origTrueValue);
			}
			elektraFree (origValue);
			return true;
		}
		else if (strcasecmp (data->booleans[i].falseValue, value) == 0)
		{
			elektraKeySetString (key, "0");
			if (data->booleanRestore != -2)
			{
				elektraKeySetMeta (key, "origvalue", origFalseValue);
			}
			elektraFree (origValue);
			return true;
		}
	}

	elektraFree (origValue);
	return false;
}

bool elektraTypeCheckBoolean (const ElektraKey * key)
{
	const char * value = elektraKeyString (key);
	return (value[0] == '1' || value[0] == '0') && value[1] == '\0';
}

bool elektraTypeRestoreBoolean (Plugin * handle ELEKTRA_UNUSED, ElektraKey * key)
{
	const ElektraKey * orig = elektraKeyGetMeta (key, "origvalue");
	if (orig != NULL)
	{
		elektraKeySetString (key, elektraKeyString (orig));
	}

	return true;
}

bool elektraTypeCheckFloat (const ElektraKey * key)
{
	kdb_float_t value;
	CHECK_TYPE (key, value, elektraKeyToFloat)
	return true;
}

bool elektraTypeCheckDouble (const ElektraKey * key)
{
	kdb_double_t value;
	CHECK_TYPE (key, value, elektraKeyToDouble)
	return true;
}

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
bool elektraTypeCheckLongDouble (const ElektraKey * key)
{
	kdb_long_double_t value;
	CHECK_TYPE (key, value, elektraKeyToLongDouble)
	return true;
}

#endif

bool elektraTypeCheckShort (const ElektraKey * key)
{
	kdb_short_t value;
	CHECK_TYPE (key, value, elektraKeyToShort)
	CHECK_TYPE_REVERSIBLE (key, value, elektraShortToString);
	return true;
}

bool elektraTypeCheckLong (const ElektraKey * key)
{
	kdb_long_t value;
	CHECK_TYPE (key, value, elektraKeyToLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraLongToString);
	return true;
}

bool elektraTypeCheckLongLong (const ElektraKey * key)
{
	kdb_long_long_t value;
	CHECK_TYPE (key, value, elektraKeyToLongLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraLongLongToString);
	return true;
}

bool elektraTypeCheckUnsignedShort (const ElektraKey * key)
{
	kdb_unsigned_short_t value;
	CHECK_TYPE (key, value, elektraKeyToUnsignedShort)
	CHECK_TYPE_REVERSIBLE (key, value, elektraUnsignedShortToString);
	return true;
}

bool elektraTypeCheckUnsignedLong (const ElektraKey * key)
{
	kdb_unsigned_long_t value;
	CHECK_TYPE (key, value, elektraKeyToUnsignedLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraUnsignedLongToString);
	return true;
}

bool elektraTypeCheckUnsignedLongLong (const ElektraKey * key)
{
	kdb_unsigned_long_long_t value;
	CHECK_TYPE (key, value, elektraKeyToUnsignedLongLong)
	CHECK_TYPE_REVERSIBLE (key, value, elektraUnsignedLongLongToString);
	return true;
}

static bool enumValidValues (const ElektraKey * key, ElektraKeyset * validValues, char * delim)
{

	const ElektraKey * maxKey = elektraKeyGetMeta (key, "check/enum");
	const char * max = maxKey == NULL ? NULL : elektraKeyString (maxKey);

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
		const ElektraKey * enumKey = elektraKeyGetMeta (key, elem);
		const char * name = enumKey != NULL ? elektraKeyString (enumKey) : "";
		if (strlen (name) > 0)
		{
			kdb_unsigned_long_long_t val = index;
			ElektraKey * k = elektraKeyNew ("user:/", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (kdb_unsigned_long_long_t), ELEKTRA_KEY_VALUE, &val, ELEKTRA_KEY_END);
			elektraKeyAddName (k, name);
			elektraKeysetAppendKey (validValues, k);
		}

		++index;
		elektraWriteArrayNumber (indexStart, index);
	}

	const ElektraKey * multiEnum = elektraKeyGetMeta (key, "check/enum/delimiter");
	if (multiEnum != NULL)
	{
		const char * delimString = elektraKeyString (multiEnum);

		if (strlen (delimString) != 1)
		{
			elektraKeysetDel (validValues);
			return false;
		}
		*delim = delimString[0];
	}

	return true;
}

static char * calculateStringValue (ElektraKeyset * validValues, char delimiter, kdb_unsigned_long_long_t value)
{
	char * stringValue = elektraStrDup ("");

	elektraKeysetRewind (validValues);
	ElektraKey * cur = NULL;
	while ((cur = elektraKeysetNext (validValues)) != NULL)
	{
		const kdb_unsigned_long_long_t * val = elektraKeyValue (cur);
		const char * name = elektraKeyName (cur) + sizeof ("user:/") - 1;
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

bool elektraTypeNormalizeEnum (Plugin * handle ELEKTRA_UNUSED, ElektraKey * key)
{
	const ElektraKey * normalize = elektraKeyGetMeta (key, "check/enum/normalize");
	if (normalize == NULL || strcmp (elektraKeyString (normalize), "1") != 0)
	{
		return true;
	}

	ElektraKeyset * validValues = elektraKeysetNew (0, ELEKTRA_KS_END);
	char delim = 0;
	if (!enumValidValues (key, validValues, &delim))
	{
		return false;
	}

	char * values = elektraStrDup (elektraKeyString (key));
	char * value = values;
	char * next;

	if (isdigit (values[0]))
	{
		kdb_unsigned_long_long_t val = ELEKTRA_UNSIGNED_LONG_LONG_S (values, NULL, 10);
		char * origValue = calculateStringValue (validValues, delim, val);
		if (origValue == NULL)
		{
			elektraKeysetDel (validValues);
			elektraFree (values);
			return false;
		}

		elektraKeySetMeta (key, "origvalue", origValue);

		elektraFree (origValue);
		elektraKeysetDel (validValues);
		elektraFree (values);
		return true;
	}

	ElektraKey * valueKey = elektraKeyNew ("user:/0", ELEKTRA_KEY_END);

	kdb_unsigned_long_long_t normalized = 0;
	if (delim != 0)
	{
		while ((next = strchr (value, delim)) != NULL)
		{
			*next = '\0';
			elektraKeySetBaseName (valueKey, value);
			ElektraKey * cur = elektraKeysetLookup (validValues, valueKey, 0);
			if (cur == NULL)
			{
				elektraKeyDel (valueKey);
				elektraKeysetDel (validValues);
				elektraFree (values);
				return false;
			}

			const kdb_unsigned_long_long_t * val = elektraKeyValue (cur);
			normalized |= *val;
			value = next + 1;
		}
	}

	elektraKeySetBaseName (valueKey, value);
	ElektraKey * cur = elektraKeysetLookup (validValues, valueKey, 0);
	elektraKeyDel (valueKey);
	if (cur == NULL)
	{
		elektraKeysetDel (validValues);
		elektraFree (values);
		return false;
	}

	const kdb_unsigned_long_long_t * val = elektraKeyValue (cur);
	normalized |= *val;

	elektraKeysetDel (validValues);
	elektraFree (values);

	char * origValue = elektraStrDup (elektraKeyString (key));
	char * normValue = elektraFormat (ELEKTRA_UNSIGNED_LONG_LONG_F, normalized);

	elektraKeySetString (key, normValue);
	elektraKeySetMeta (key, "origvalue", origValue);

	elektraFree (origValue);
	elektraFree (normValue);

	return true;
}

bool elektraTypeCheckEnum (const ElektraKey * key)
{
	const ElektraKey * normalize = elektraKeyGetMeta (key, "check/enum/normalize");
	if (normalize != NULL && strcmp (elektraKeyString (normalize), "1") == 0)
	{
		// was already implicitly checked during normalization
		return true;
	}

	const ElektraKey * maxKey = elektraKeyGetMeta (key, "check/enum");
	const char * max = maxKey == NULL ? NULL : elektraKeyString (maxKey);

	if (max == NULL)
	{
		return false;
	}

	ElektraKeyset * validValues = elektraKeysetNew (0, ELEKTRA_KS_END);
	char delim = 0;
	if (!enumValidValues (key, validValues, &delim))
	{
		return false;
	}

	char * values = elektraStrDup (elektraKeyString (key));
	char * value = values;
	char * next;

	ElektraKey * valueKey = elektraKeyNew ("user:/0", ELEKTRA_KEY_END);

	if (delim != 0)
	{
		while ((next = strchr (value, delim)) != NULL)
		{
			*next = '\0';
			elektraKeySetBaseName (valueKey, value);
			if (elektraKeysetLookup (validValues, valueKey, 0) == NULL)
			{
				elektraKeyDel (valueKey);
				elektraKeysetDel (validValues);
				elektraFree (values);
				return false;
			}
			value = next + 1;
		}
	}

	elektraKeySetBaseName (valueKey, value);
	if (elektraKeysetLookup (validValues, valueKey, 0) == NULL)
	{
		elektraKeyDel (valueKey);
		elektraKeysetDel (validValues);
		elektraFree (values);
		return false;
	}


	elektraKeyDel (valueKey);
	elektraKeysetDel (validValues);
	elektraFree (values);

	return true;
}

bool elektraTypeRestoreEnum (Plugin * handle ELEKTRA_UNUSED, ElektraKey * key)
{
	const ElektraKey * orig = elektraKeyGetMeta (key, "origvalue");
	if (orig != NULL)
	{
		elektraKeySetString (key, elektraKeyString (orig));
	}

	return true;
}

void elektraTypeSetErrorEnum (Plugin * handle ELEKTRA_UNUSED, ElektraKey * errorKey, const ElektraKey * key)
{
	const ElektraKey * maxKey = elektraKeyGetMeta (key, "check/enum");
	const char * max = maxKey == NULL ? NULL : elektraKeyString (maxKey);

	if (max == NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
			errorKey,
			"The type 'enum' failed to match for '%s' with string: '%s'\n"
			"No values are allowed (check/enum is an empty array, or parent isn't set to last element)",
			elektraKeyName (key), elektraKeyString (key));
		return;
	}

	char * errorMessage = elektraFormat (
		"The type 'enum' failed to match for '%s' with string: '%s'\n"
		"Allowed values:",
		elektraKeyName (key), elektraKeyString (key));

	char elem[sizeof ("check/enum/") + ELEKTRA_MAX_ARRAY_SIZE];
	strcpy (elem, "check/enum/");
	char * indexStart = elem + sizeof ("check/enum/") - 1;

	kdb_long_long_t index = 0;
	elektraWriteArrayNumber (indexStart, index);
	while (strcmp (indexStart, max) <= 0)
	{
		const ElektraKey * enumKey = elektraKeyGetMeta (key, elem);
		const char * name = enumKey != NULL ? elektraKeyString (enumKey) : "";
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
