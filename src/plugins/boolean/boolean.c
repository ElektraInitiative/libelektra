/**
 * @file
 *
 * @brief Source for boolean plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "boolean.h"

#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#define DEFAULT_TRUE_VALUE "1"
#define DEFAULT_FALSE_VALUE "0"

typedef struct
{
	const char * trueValue;
	const char * falseValue;
	const char * invalidValue;
	bool warnInvalid;
	char ** trueValues;
	char ** falseValues;
} BoolData;

static const char * defaultTrueValues[] = {
	"TRUE", "1", "ON", "ENABLE", "ENABLED", "YES", NULL,
};
static const char * defaultFalseValues[] = {
	"FALSE", "0", "OFF", "DISABLE", "DISABLED", "NO", "NOT", NULL,
};

int elektraBooleanClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional
	BoolData * data = elektraPluginGetData (handle);
	if (!data)
	{
		return 1;
	}
	if (data->trueValues)
	{
		char ** ptr = (data->trueValues);
		while (*ptr)
		{
			elektraFree (*ptr);
			++(ptr);
		}
		elektraFree (data->trueValues);
	}
	if (data->falseValues)
	{
		char ** ptr = (data->falseValues);
		while (*ptr)
		{
			elektraFree (*ptr);
			++(ptr);
		}
		elektraFree (data->falseValues);
	}
	elektraFree (data);
	elektraPluginSetData (handle, NULL);
	return 1; // success
}

static int isTrue (const char * value, const char ** trueValues)
{
	char ** ptr = (char **) trueValues;
	int retVal = 0;
	while (*ptr)
	{
		if (!strcasecmp (value, *ptr))
		{
			retVal = 1;
			break;
		}
		++ptr;
	}
	return retVal;
}

static int isFalse (const char * value, const char ** falseValues)
{
	char ** ptr = (char **) falseValues;
	int retVal = 0;
	while (*ptr)
	{
		if (!strcasecmp (value, *ptr))
		{
			retVal = 1;
			break;
		}
		++ptr;
	}
	return retVal;
}

static void strToArray (Key * key, char *** array)
{
	int count = 1;
	const char * values = keyString (key);
	char * ptr = (char *) values;
	while (*ptr)
	{
		if (*ptr == ';')
		{
			++count;
		}
		++ptr;
	}
	*array = elektraCalloc ((count + 1) * sizeof (char *));
	char * localString = elektraStrDup (values);
	char * saveptr = 0;
	char * token = 0;
	token = strtok_r (localString, ";", &saveptr);
	if (!token)
	{
		elektraFree (array);
	}
	else
	{
		int index = 0;
		ptr = token;
		while (*ptr == ' ')
			++ptr;
		(*array)[index++] = elektraStrDup (ptr);
		while ((token = strtok_r (0, ";", &saveptr)) != NULL)
		{
			ptr = token;
			while (*ptr == ' ')
				++ptr;
			(*array)[index++] = elektraStrDup (ptr);
		}
	}
	elektraFree (localString);
}

static void parseConfig (KeySet * config, BoolData * data)
{
	Key * trueKey = ksLookupByName (config, "/on/true", 0);
	Key * falseKey = ksLookupByName (config, "/on/false", 0);

	data->trueValue = trueKey ? keyString (trueKey) : DEFAULT_TRUE_VALUE;
	data->falseValue = falseKey ? keyString (falseKey) : DEFAULT_FALSE_VALUE;

	Key * invalidKey = ksLookupByName (config, "/on/invalid", 0);
	Key * invalidWarningKey = ksLookupByName (config, "/on/invalid/warning", 0);

	data->warnInvalid = true;
	data->invalidValue = data->trueValue;

	if (invalidKey)
	{
		if (!strcasecmp (keyString (invalidKey), "FALSE"))
		{
			data->invalidValue = data->falseValue;
		}
		else
		{
			data->invalidValue = data->trueValue;
		}
	}

	if (invalidWarningKey)
	{
		if (!strcasecmp (keyString (invalidWarningKey), "FALSE"))
		{
			data->warnInvalid = false;
		}
		else
		{
			data->warnInvalid = true;
		}
	}

	data->trueValues = NULL;
	data->falseValues = NULL;

	Key * validTrueKey = ksLookupByName (config, "/true", 0);
	if (validTrueKey)
	{
		strToArray (validTrueKey, &data->trueValues);
	}

	Key * validFalseKey = ksLookupByName (config, "/false", 0);
	if (validFalseKey)
	{
		strToArray (validFalseKey, &data->falseValues);
	}
}

static int isBool (const Key * key)
{
	const Key * boolMeta = keyGetMeta (key, "type");
	if (boolMeta)
	{
		ELEKTRA_LOG_DEBUG ("Meta key “type” contains value “%s”", keyString (boolMeta));
		return !strcmp (keyString (boolMeta), "boolean");
	}

	boolMeta = keyGetMeta (key, "check/type");
	if (boolMeta && !strcmp (keyString (boolMeta), "boolean"))
	{
		return 1;
	}
	return 0;
}

int elektraBooleanGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/boolean"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/boolean", KEY_VALUE, "boolean plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/boolean/exports", KEY_END),
			       keyNew ("system/elektra/modules/boolean/exports/close", KEY_FUNC, elektraBooleanClose, KEY_END),
			       keyNew ("system/elektra/modules/boolean/exports/get", KEY_FUNC, elektraBooleanGet, KEY_END),
			       keyNew ("system/elektra/modules/boolean/exports/set", KEY_FUNC, elektraBooleanSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/boolean/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	BoolData * data = elektraPluginGetData (handle);
	if (!data)
	{
		KeySet * config = elektraPluginGetConfig (handle);
		data = elektraCalloc (sizeof (BoolData));
		parseConfig (config, data);
		elektraPluginSetData (handle, data);
	}

	const char ** trueValues = data->trueValues != NULL ? (const char **) data->trueValues : defaultTrueValues;
	const char ** falseValues = data->falseValues != NULL ? (const char **) data->falseValues : defaultFalseValues;

	Key * key;
	ksRewind (returned);
	while ((key = ksNext (returned)) != NULL)
	{
		uint8_t isBoolean = isBool (key);
		ELEKTRA_LOG_DEBUG ("Key “%s” %s a boolean", keyName (key), isBoolean ? "contains" : "does not contain");
		if (isBoolean)
		{
			const char * value = keyString (key);

			if (isTrue (value, trueValues))
			{
				keySetMeta (key, "internal/boolean/origvalue", keyString (key));
				keySetMeta (key, "internal/boolean/normvalue", data->trueValue);
				ELEKTRA_LOG_DEBUG ("Convert “%s” to “%s”", value, data->trueValue);
				keySetString (key, data->trueValue);
			}
			else if (isFalse (value, falseValues))
			{
				keySetMeta (key, "internal/boolean/origvalue", keyString (key));
				keySetMeta (key, "internal/boolean/normvalue", data->falseValue);
				ELEKTRA_LOG_DEBUG ("Convert “%s” to “%s”", value, data->falseValue);
				keySetString (key, data->falseValue);
			}
			else
			{
				ELEKTRA_LOG_DEBUG ("Neither true nor false value");
				if (data->warnInvalid)
				{
					ELEKTRA_ADD_WARNINGF (ELEKTRA_WARNING_INVALID_BOOL, parentKey,
							      "Key %s with value %s is not a valid boolean. Defaulting to %s.",
							      keyName (key), keyString (key), data->invalidValue);
				}
				keySetMeta (key, "internal/boolean/origvalue", keyString (key));
				keySetString (key, data->invalidValue);
			}
		}
	}
	return 1; // success
}

int elektraBooleanSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	// get all keys
	// this function is optional

	BoolData * data = elektraPluginGetData (handle);
	if (!data)
	{
		KeySet * config = elektraPluginGetConfig (handle);
		data = elektraCalloc (sizeof (BoolData));
		parseConfig (config, data);
		elektraPluginSetData (handle, data);
	}

	const char ** trueValues = data->trueValues != NULL ? (const char **) data->trueValues : defaultTrueValues;
	const char ** falseValues = data->falseValues != NULL ? (const char **) data->falseValues : defaultFalseValues;

	ksRewind (returned);
	Key * key;
	while ((key = ksNext (returned)) != NULL)
	{
		uint8_t isBoolean = isBool (key);
		ELEKTRA_LOG_DEBUG ("Key “%s” %s a boolean", keyName (key), isBoolean ? "contains" : "does not contain");
		if (isBoolean)
		{
			const char * value = keyString (key);
			const Key * origValue = keyGetMeta (key, "internal/boolean/origvalue");
			const char * originalValue = origValue == NULL ? NULL : keyString (origValue);
			const Key * norm = keyGetMeta (key, "internal/boolean/normvalue");
			const char * normValue = norm == NULL ? NULL : keyString (norm);

			if (norm == NULL && strcmp (value, data->invalidValue) == 0)
			{
				// unchanged invalid value
				ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_BOOL, parentKey, "%s is not a valid boolean value",
						    keyString (key));
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}


			if (normValue != NULL && strcmp (value, normValue) == 0)
			{
				// unchanged valid (no warning) normalized value
				keySetString (key, originalValue);
				keySetMeta (key, "internal/boolean/origvalue", NULL);
				keySetMeta (key, "internal/boolean/normvalue", NULL);
				continue;
			}

			if (isTrue (value, trueValues))
			{
				if (originalValue != NULL && isTrue (originalValue, trueValues))
				{
					keySetString (key, originalValue);
				}
			}
			else if (isFalse (value, falseValues))
			{
				if (originalValue != NULL && isFalse (originalValue, trueValues))
				{
					keySetString (key, originalValue);
				}
			}
			else
			{
				ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_BOOL, parentKey, "%s is not a valid boolean value",
						    keyString (key));
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			keySetMeta (key, "internal/boolean/origvalue", NULL);
			keySetMeta (key, "internal/boolean/normvalue", NULL);
		}
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
    return elektraPluginExport ("boolean",
	    ELEKTRA_PLUGIN_CLOSE,	&elektraBooleanClose,
	    ELEKTRA_PLUGIN_GET,	&elektraBooleanGet,
	    ELEKTRA_PLUGIN_SET,	&elektraBooleanSet,
	    ELEKTRA_PLUGIN_END);
}
