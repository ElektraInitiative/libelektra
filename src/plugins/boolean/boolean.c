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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>


#define DEFAULT_TRUE_VALUE "1"
#define DEFAULT_FALSE_VALUE "0"

typedef enum
{
	TRUE = 1 << 0,
	FALSE = 1 << 1,
	WARNING = 1 << 2,
} InvalidAction;

typedef struct
{
	char * trueValue;
	char * falseValue;
	InvalidAction invalid;
	char ** trueValues;
	char ** falseValues;
} BoolData;

int elektraBooleanClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional
	BoolData * data = elektraPluginGetData (handle);
	if (!data) return 1;
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

static void normalize (Key * key, Key * parentKey, BoolData * data)
{
	const char * defaultTrueValues[] = {
		"TRUE", "1", "ON", "ENABLE", "ENABLED", "YES", NULL,
	};
	const char * defaultFalseValues[] = {
		"FALSE", "0", "OFF", "DISABLE", "DISABLED", "NO", "NOT", NULL,
	};

	char * origvalue = elektraStrDup (keyString (key));
	const char ** falseValues = (const char **) data->falseValues;
	const char ** trueValues = (const char **) data->trueValues;
	const char ** falseStrings = falseValues ? falseValues : defaultFalseValues;
	const char ** trueStrings = trueValues ? trueValues : defaultTrueValues;
	if (isTrue (origvalue, trueStrings))
	{
		ELEKTRA_LOG_DEBUG ("Convert “%s” to “%s”", origvalue, data->trueValue);
		keySetString (key, data->trueValue);
		keySetMeta (key, "origvalue", origvalue);
	}
	else if (isFalse (origvalue, falseStrings))
	{
		ELEKTRA_LOG_DEBUG ("Convert “%s” to “%s”", origvalue, data->falseValue);
		keySetString (key, data->falseValue);
		keySetMeta (key, "origvalue", origvalue);
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("Neither true nor false value");
		keySetMeta (key, "boolean/invalid", "");
		switch ((data->invalid & ~(WARNING)))
		{
		case TRUE:
			if (data->invalid & WARNING)
			{
				ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (parentKey,
									  "Key %s with value '%s' is not a valid boolean. Defaulting to %s",
									  keyName (key), origvalue, data->trueValue);
			}
			keySetMeta (key, "origvalue", origvalue);
			keySetString (key, data->trueValue);
			break;
		case FALSE:
			if (data->invalid & WARNING)
			{
				ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (parentKey,
									  "Key %s with value '%s' is not a valid boolean. Defaulting to %s",
									  keyName (key), origvalue, data->falseValue);
			}
			keySetString (key, data->falseValue);
			keySetMeta (key, "origvalue", origvalue);
			break;
		case WARNING:
			break;
		}
	}
	elektraFree (origvalue);
}

static void strToArray (Key * key, char *** array)
{
	int count = 1;
	const char * values = keyString (key);
	char * ptr = (char *) values;
	while (*ptr)
	{
		if (*ptr == ';') ++count;
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
		array = NULL;
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
	const char * trueValue = trueKey ? keyString (trueKey) : DEFAULT_TRUE_VALUE;
	const char * falseValue = falseKey ? keyString (falseKey) : DEFAULT_FALSE_VALUE;

	Key * invalidKey = ksLookupByName (config, "/on/invalid", 0);
	Key * invalidWarningKey = ksLookupByName (config, "/on/invalid/warning", 0);
	data->invalid = 0;
	if (!invalidKey && !invalidWarningKey)
	{
		data->invalid = (TRUE | WARNING);
	}
	if (invalidKey)
	{
		if (!strcasecmp (keyString (invalidKey), "FALSE"))
		{
			data->invalid |= FALSE;
		}
		else
		{
			data->invalid |= TRUE;
		}
	}
	if (invalidWarningKey)
	{
		if (!strcasecmp (keyString (invalidWarningKey), "TRUE"))
			data->invalid |= WARNING;
		else if (!strcasecmp (keyString (invalidWarningKey), "FALSE"))
			data->invalid &= WARNING;
		else
			data->invalid |= WARNING;
	}
	data->trueValue = (char *) trueValue;
	data->falseValue = (char *) falseValue;
	Key * validTrueKey = ksLookupByName (config, "/true", 0);
	Key * validFalseKey = ksLookupByName (config, "/false", 0);
	if (validTrueKey)
	{
		strToArray (validTrueKey, &data->trueValues);
	}
	else
	{
		data->trueValues = NULL;
	}
	if (validFalseKey)
	{
		strToArray (validFalseKey, &data->falseValues);
	}
	else
	{
		data->falseValues = NULL;
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
	if (boolMeta && !strcmp (keyString (boolMeta), "boolean")) return 1;
	return 0;
}

int elektraBooleanGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
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
	Key * key;
	ksRewind (returned);
	while ((key = ksNext (returned)) != NULL)
	{
		uint8_t isBoolean = isBool (key);
		ELEKTRA_LOG_DEBUG ("Key “%s” %s a boolean", keyName (key), isBoolean ? "contains" : "does not contain");
		if (isBoolean)
		{
			normalize (key, parentKey, data);
		}
	}
	return 1; // success
}

static void restoreValue (Key * key, const char * origValue)
{
	keySetString (key, origValue);
}

int elektraBooleanSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
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
	const char * trueValue = data->trueValue;
	const char * falseValue = data->falseValue;

	ksRewind (returned);
	Key * key;
	int retVal = 1;
	while ((key = ksNext (returned)) != NULL)
	{
		uint8_t isBoolean = isBool (key);
		ELEKTRA_LOG_DEBUG ("Key “%s” %s a boolean", keyName (key), isBoolean ? "contains" : "does not contain");
		if (isBoolean)
		{
			if (!keyGetMeta (key, "origvalue")) normalize (key, parentKey, data);
			const Key * originalValue = keyGetMeta (key, "origvalue");
			if (!(!strcmp (keyString (key), trueValue) || !strcmp (keyString (key), falseValue)) ||
			    (keyGetMeta (key, "boolean/invalid")))
			{
				keySetMeta (key, "boolean/invalid", 0);
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "String '%s' is not a valid boolean value",
									keyString (originalValue));
				retVal = -1;
			}
			if (originalValue) restoreValue (key, keyString (originalValue));
		}
	}
	return retVal; // success
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
