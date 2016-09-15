/**
 * @file
 *
 * @brief Source for boolean plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "boolean.h"

#include <ctype.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define DEFAULT_TRUE_VALUE "1"
#define DEFAULT_FALSE_VALUE "0"

typedef enum {
	TRUE = 1 << 0,
	FALSE = 1 << 1,
	WARNING = 1 << 2,
} InvalidAction;

typedef struct
{
	char * true;
	char * false;
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

static int stricmp (const char * s1, const char * s2)
{

	if (!(*s1) && !(*s2))
	{
		return 0;
	}
	else if (!(*s1))
	{
		return (-1);
	}
	else if (!(*s2))
	{
		return 1;
	}
	else
	{
		char * p1 = (char *)s1;
		char * p2 = (char *)s2;
		while (1)
		{
			if (!(*p1) && !(*p2))
			{
				return 0;
			}
			else if (!(*p1))
			{
				return (-1);
			}
			else if (!(*p2))
			{
				return 1;
			}
			else
			{
				if (toupper (*p1) == toupper (*p2))
				{
					++p1;
					++p2;
				}
				else
				{
					return (toupper (*p1) - toupper (*p2));
				}
			}
		}
	}
}

static int isTrue (const char * value, const char ** trueValues)
{
	char ** ptr = (char **)trueValues;
	int retVal = 0;
	while (*ptr)
	{
		if (!stricmp (value, *ptr))
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
	char ** ptr = (char **)falseValues;
	int retVal = 0;
	while (*ptr)
	{
		if (!stricmp (value, *ptr))
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
	const char * value = keyString (key);
	const char ** falseValues = (const char **)data->falseValues;
	const char ** trueValues = (const char **)data->trueValues;
	const char ** falseStrings = falseValues ? falseValues : defaultFalseValues;
	const char ** trueStrings = trueValues ? trueValues : defaultTrueValues;
	if (isTrue (value, trueStrings))
	{
		keySetMeta (key, "origvalue", keyString (key));
		keySetString (key, data->true);
	}
	else if (isFalse (value, falseStrings))
	{
		keySetMeta (key, "origvalue", keyString (key));
		keySetString (key, data->false);
	}
	else
	{
		keySetMeta (key, "boolean/invalid", "");
		switch ((data->invalid & ~(WARNING)))
		{
		case TRUE:
			if (data->invalid & WARNING)
			{
				ELEKTRA_ADD_WARNINGF (ELEKTRA_WARNING_INVALID_BOOL, parentKey,
						      "Key %s with value %s is not a valid boolean. Defaulting to %s.", keyName (key),
						      keyString (key), data->true);
			}
			keySetMeta (key, "originalvalue", keyString (key));
			keySetString (key, data->true);
			break;
		case FALSE:
			if (data->invalid & WARNING)
			{
				ELEKTRA_ADD_WARNINGF (ELEKTRA_WARNING_INVALID_BOOL, parentKey,
						      "Key %s with value %s is not a valid boolean. Defaulting to %s.", keyName (key),
						      keyString (key), data->false);
			}
			keySetMeta (key, "origvalue", keyString (key));
			keySetString (key, data->false);
			break;
		case WARNING:
			break;
		}
	}
}

static void strToArray (Key * key, char *** array)
{
	int count = 1;
	const char * values = keyString (key);
	char * ptr = (char *)values;
	while (*ptr)
	{
		if (*ptr == ';') ++count;
		++ptr;
	}
	*array = elektraCalloc ((count + 1) * sizeof (char *));
	char * localString = strdup (values);
	char * saveptr = 0;
	char * token = 0;
	int index = 0;
	token = strtok_r (localString, ";", &saveptr);
	if (!token)
	{
		elektraFree (array);
		array = NULL;
	}
	else
	{
		ptr = token;
		while (*ptr == ' ')
			++ptr;
		(*array)[index++] = strdup (ptr);
		while ((token = strtok_r (0, ";", &saveptr)) != NULL)
		{
			ptr = token;
			while (*ptr == ' ')
				++ptr;
			(*array)[index++] = strdup (ptr);
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
		if (!stricmp (keyString (invalidKey), "FALSE"))
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
		if (!stricmp (keyString (invalidWarningKey), "TRUE"))
			data->invalid |= WARNING;
		else if (!stricmp (keyString (invalidWarningKey), "FALSE"))
			data->invalid &= WARNING;
		else
			data->invalid |= WARNING;
	}
	data->true = (char *)trueValue;
	data->false = (char *)falseValue;
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
#include ELEKTRA_README (boolean)
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
		const Key * boolMeta = keyGetMeta (key, "type");
		if (!boolMeta) continue;
		if (!strcmp (keyString (boolMeta), "boolean"))
		{
			normalize (key, parentKey, data);
		}
	}
	return 1; // success
}

static void restoreValue (Key * key, const char * origName)
{
	keySetString (key, origName);
	keySetMeta (key, "origvalue", 0);
	return;
}

int elektraBooleanSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	// this function is optional

	BoolData * data = elektraPluginGetData (handle);
	if (!data) return -1;
	const char * trueValue = data->true;
	const char * falseValue = data->false;

	ksRewind (returned);
	Key * key;
	int retVal = 1;
	while ((key = ksNext (returned)) != NULL)
	{
		const Key * checkMeta = keyGetMeta (key, "type");
		if (checkMeta)
		{
			if (!strcmp (keyString (checkMeta), "boolean"))
			{
				const Key * nameMeta = keyGetMeta (key, "origvalue");
				if ((!(!strcmp (keyString (key), trueValue) || !strcmp (keyString (key), falseValue))) ||
				    (keyGetMeta (key, "boolean/invalid")))
				{
					keySetMeta (key, "boolean/invalid", 0);
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_BOOL, parentKey, "%s is not a valid boolean value",
							    keyString (nameMeta));
					retVal = -1;
				}
				if (nameMeta) restoreValue (key, keyString (nameMeta));
			}
		}
	}
	return retVal; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (boolean)
{
	// clang-format off
    return elektraPluginExport ("boolean",
	    ELEKTRA_PLUGIN_CLOSE,	&elektraBooleanClose,
	    ELEKTRA_PLUGIN_GET,	&elektraBooleanGet,
	    ELEKTRA_PLUGIN_SET,	&elektraBooleanSet,
	    ELEKTRA_PLUGIN_END);
}

