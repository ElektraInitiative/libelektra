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

#include <ctype.h>
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

static inline void lskip (char ** p)
{
	while (**p && isspace (**p))
		++(*p);
}

static inline void rstrip (const char * s, char ** p)
{
	while ((*p > s) && **p && isspace (**p))
	{
		**p = '\0';
		--(*p);
	}
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
	rstrip (sPtr, &ePtr);
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
	sPtr = cPtr + 1;
	lskip (&sPtr);
	ePtr = strrchr (sPtr, ')');
	*ePtr-- = '\0';
	rstrip (sPtr, &ePtr);
	size_t argLen = ePtr - sPtr + 1;
	char * prefArg = elektraMalloc (argLen + 1);
	snprintf (prefArg, argLen + 1, "%s", sPtr);
	if (!strcmp (prefArg, "true") || !(strcmp (prefArg, "false")))
	{
		keySetMeta (key, "type", "boolean");
		keySetString (key, prefArg);
	}
	else if (prefArg[0] == '"' && prefArg[strlen (prefArg) - 1] == '"')
	{
		// TODO: else if list
		keySetMeta (key, "type", "string");
		*prefArg = '\0';
		*(prefArg + (strlen (prefArg + 1))) = '\0';
		keySetString (key, (prefArg + 1));
	}
	else
	{
		keySetMeta (key, "type", "integer");
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

int elektraPrefsGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
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
			fseek (fp, ((len - 1) * (-1)), SEEK_CUR);
			len *= 2;
			elektraRealloc ((void **)&buffer, len * sizeof (char));
			continue;
		}
		else
		{
			buffer[strlen (buffer) - 1] = '\0';
		}
		char * ptr = buffer;
		lskip (&ptr);
		if (!strncmp (buffer, "//", 2)) continue;
		for (DataType d = VAR; d < DATA_END; ++d)
		{
			if (!strncmp (ptr, data[d], strlen (data[d])))
			{
				key = varToKey (parentKey, d, ptr);
				ksAppendKey (returned, key);
				goto LOOP_END;
			}
		}
		for (PrefType p = PREF; p < PREF_END; ++p)
		{
			if (!strncmp (ptr, function[p], strlen (function[p])))
			{
				key = prefToKey (parentKey, p, ptr + strlen (function[p]));
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

static char * keyNameToPrefName (const char * prefName)
{
	char * buffer = elektraCalloc (strlen (prefName) + 1);
	char * src = (char *)prefName;
	char * dst = buffer;
	unsigned short flag = 0;
	while (*src)
	{
		switch (*src)
		{
		case '\\':
			if (flag)
			{
				*dst++ = *src;
				flag = 0;
			}
			else
				flag = 1;
			break;
		case '/':
			*dst++ = '.';
			break;
		default:
			*dst++ = *src;
			break;
		}
		++src;
	}
	return buffer;
}

static inline const char * prefTypToFunction (PrefType pref)
{
	if (pref >= PREF_END) return NULL;
	return function[pref];
}

static char * prefArgToString (const Key * key)
{
	const Key * typeMeta = keyGetMeta (key, "type");
	if (!typeMeta) return NULL;
	char * buffer = NULL;
	if (!strcmp (keyString (typeMeta), "boolean"))
	{
		buffer = strdup (keyString (key));
	}
	else if (!strcmp (keyString (typeMeta), "string"))
	{
		ssize_t len = keyGetValueSize (key) + 2; // size of string + leading and trailing '"'
		buffer = elektraCalloc (len);
		snprintf (buffer, len, "\"%s\"", keyString (key));
	}
	else if (!strcmp (keyString (typeMeta), "integer"))
	{
		buffer = strdup (keyString (key));
	}
	return buffer;
}

static void writeKey (FILE * fp, const Key * parentKey, const Key * key)
{
	char * prefName = (char *)keyName (key) + strlen (keyName (parentKey)) + 1; // skip parentKey name + '/'
	if (strncmp (prefName, "preferences", sizeof ("preferences") - 1))
		return;
	else
		prefName += sizeof ("preferences");
	unsigned short flag = 0;
	PrefType pref = PREF;
	for (; pref < PREF_END; ++pref)
	{
		if (!strncmp (prefName, prefix[pref], strlen (prefix[pref])))
		{
			flag = 1;
			prefName += strlen (prefix[pref]) + 1; // skip prefix len + '/'
			break;
		}
	}
	if (!flag) return;

	char * realPrefName = keyNameToPrefName (prefName);
	if (!realPrefName) return;
	const char * functionName = prefTypToFunction (pref);
	char * argString = NULL;
	if (!functionName) goto WRITE_CLEANUP;
	argString = prefArgToString (key);
	if (!argString) goto WRITE_CLEANUP;
	fprintf (fp, "%s(\"%s\", %s);\n", functionName, realPrefName, argString);
WRITE_CLEANUP:
	if (realPrefName) elektraFree (realPrefName);
	if (argString) elektraFree (argString);
}

int elektraPrefsSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	// get all keys
	// this function is optional

	FILE * fp = fopen (keyString (parentKey), "w");
	if (!fp) return -1;
	fprintf(fp, "//\n");
	Key * cur;
	while ((cur = ksNext (returned)) != NULL)
	{
		if (!strcmp (keyName (parentKey), keyName (cur))) continue;
		writeKey (fp, parentKey, cur);
	}
	fclose(fp);
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

