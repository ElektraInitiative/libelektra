/**
 * @file
 *
 * @brief Source for prefs plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "prefs.h"

#include <kdbhelper.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
	PREF = 0,
	USER,
	LOCK,
	STICKY,
	PREF_END,
} PrefType;
typedef enum {
	VAR = 0,
	CONST,
	DATA_END,
} DataType;

const char * data[] = { "var", "const" };

const char * function[] = { "pref", "user_pref", "lock_pref", "sticky_pref" };
const char * prefix[] = { "pref", "user", "lock", "sticky" };

int elektraPrefsOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return 1; // success
}

int elektraPrefsClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return 1; // success
}

static Key * prefToKey (Key * parentKey, PrefType type, const char * pref)
{
	Key * key = keyNew (keyName (parentKey), KEY_END);
	keyAddBaseName (key, "preferences");
	keyAddBaseName (key, prefix[type]);
	char * localString = strdup (pref);
	char * cPtr = strstr (localString, ",");
	*cPtr = '\0';
	char * sPtr = localString;
	++sPtr;
	*sPtr++ = '\0';
	char * ePtr = cPtr - 1;
	*ePtr = '\0';
	size_t keyLen = ePtr - sPtr;
	char * prefKey = elektraMalloc (keyLen + 1);
	snprintf (prefKey, keyLen + 1, "%s", sPtr);
	char * tPtr = strtok (prefKey, ".");
	if (tPtr) keyAddBaseName (key, tPtr);
	while ((tPtr = strtok (NULL, ".")) != NULL)
	{
		keyAddBaseName (key, tPtr);
	}
	elektraFree (prefKey);
	sPtr = cPtr + 2;
	ePtr = strrchr (sPtr, ')');
	*ePtr = '\0';
	size_t argLen = ePtr - sPtr;
	char * prefArg = elektraMalloc (argLen + 1);
	snprintf (prefArg, argLen + 1, "%s", sPtr);
	if (!strcmp (prefArg, "true") || !(strcmp (prefArg, "false")))
	{
		keySetMeta (key, "pref/type", "boolean");
		keySetString (key, prefArg);
	}
	else if (prefArg[0] == '"' && prefArg[strlen (prefArg) - 1] == '"')
	{
		// TODO: else if list
		keySetMeta (key, "pref/type", "string");
		*prefArg = '\0';
		*(prefArg + (strlen (prefArg + 1))) = '\0';
		keySetString (key, (prefArg + 1));
	}
	else
	{
		keySetMeta (key, "pref/type", "integer");
		keySetString (key, prefArg);
	}
	elektraFree (prefArg);
	elektraFree (localString);
	return key;
}

static Key * varToKey (Key * parentKey, DataType d, const char * string)
{
	Key * key = keyNew (keyString (parentKey), KEY_END);
	keyAddBaseName (parentKey, "variables");
	return key;
}

int elektraPrefsGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/prefs"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/prefs", KEY_VALUE, "prefs plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/prefs/exports", KEY_END),
			       keyNew ("system/elektra/modules/prefs/exports/open", KEY_FUNC, elektraPrefsOpen, KEY_END),
			       keyNew ("system/elektra/modules/prefs/exports/close", KEY_FUNC, elektraPrefsClose, KEY_END),
			       keyNew ("system/elektra/modules/prefs/exports/get", KEY_FUNC, elektraPrefsGet, KEY_END),
			       keyNew ("system/elektra/modules/prefs/exports/set", KEY_FUNC, elektraPrefsSet, KEY_END),
			       keyNew ("system/elektra/modules/prefs/exports/error", KEY_FUNC, elektraPrefsError, KEY_END),
			       keyNew ("system/elektra/modules/prefs/exports/checkconf", KEY_FUNC, elektraPrefsCheckConfig, KEY_END),
#include ELEKTRA_README (prefs)
			       keyNew ("system/elektra/modules/prefs/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	const char * fileName = keyString (parentKey);
	FILE * fp = fopen (fileName, "r");
	int len = 1024;
	char * buffer = elektraMalloc (len * sizeof (char));
	Key * key;
	Key * sectionKey = keyNew (keyName (parentKey), KEY_END);
	keySetBinary (sectionKey, 0, 0);
	keyAddBaseName (sectionKey, "preferences");
	ksAppendKey (returned, keyDup (sectionKey));
	keySetBaseName (sectionKey, "variables");
	ksAppendKey (returned, keyDup (sectionKey));
	keySetBaseName (sectionKey, "functions");
	ksAppendKey (returned, keyDup (sectionKey));
	keyDel (sectionKey);

	while (fgets (buffer, len, fp))
	{
		if (buffer[strlen (buffer) - 1] != '\n')
		{
			fseek (fp, ((len - 1) * (-1)), ftell (fp));
			len *= 2;
			elektraRealloc ((void **)&buffer, len * sizeof (char));
			continue;
		}
		else
		{
			buffer[strlen (buffer) - 1] = '\0';
		}
		if (!strncmp (buffer, "//", 2)) continue;
		for (DataType d = VAR; d < DATA_END; ++d)
		{
			if (!strncmp (buffer, data[d], strlen (data[d])))
			{
				key = varToKey (parentKey, d, buffer);
				ksAppendKey (returned, key);
				goto LOOP_END;
			}
		}
		for (PrefType p = PREF; p < PREF_END; ++p)
		{
			if (!strncmp (buffer, function[p], strlen (function[p])))
			{
				key = prefToKey (parentKey, p, buffer + strlen (function[p]));
				ksAppendKey (returned, key);
				goto LOOP_END;
			}
		}
	LOOP_END:
		continue;
	}
	elektraFree (buffer);
	fclose (fp);
	return 1; // success
}

int elektraPrefsSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	// this function is optional

	return 1; // success
}

int elektraPrefsError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return 1; // success
}

int elektraPrefsCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (prefs)
{
	// clang-format off
	return elektraPluginExport ("prefs",
		ELEKTRA_PLUGIN_OPEN,	&elektraPrefsOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraPrefsClose,
		ELEKTRA_PLUGIN_GET,	&elektraPrefsGet,
		ELEKTRA_PLUGIN_SET,	&elektraPrefsSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraPrefsError,
		ELEKTRA_PLUGIN_END);
}

