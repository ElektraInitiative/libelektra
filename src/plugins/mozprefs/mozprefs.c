/**
 * @file
 *
 * @brief Source for mozprefs plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "mozprefs.h"

#include <kdbhelper.h>
#include <kdbutility.h>

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


const char * function[] = { "pref", "user_pref", "lockPref", "sticky_pref" };
const char * prefix[] = { "pref", "user", "lock", "sticky" };

static Key * prefToKey (Key * parentKey, PrefType type, const char * pref)
{
	Key * key = keyNew (keyName (parentKey), KEY_END);
	keyAddBaseName (key, prefix[type]);
	char * localString = strdup (pref);
	char * cPtr = strstr (localString, ",");
	*cPtr = '\0';
	char * sPtr = localString;
	++sPtr;
	*sPtr++ = '\0';
	char * ePtr = cPtr - 1;
	elektraRstrip (sPtr, &ePtr);
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
	sPtr = elektraLskip (sPtr);
	ePtr = strrchr (sPtr, ')');
	*ePtr-- = '\0';
	elektraRstrip (sPtr, &ePtr);
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


int elektraMozprefsGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/mozprefs"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/mozprefs", KEY_VALUE, "mozprefs plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/mozprefs/exports", KEY_END),
			       keyNew ("system/elektra/modules/mozprefs/exports/get", KEY_FUNC, elektraMozprefsGet, KEY_END),
			       keyNew ("system/elektra/modules/mozprefs/exports/set", KEY_FUNC, elektraMozprefsSet, KEY_END),
#include ELEKTRA_README (mozprefs)
			       keyNew ("system/elektra/modules/mozprefs/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
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
		ptr = elektraLskip (ptr);
		if (!strncmp (buffer, "//", 2)) continue;
		for (PrefType p = PREF; p < PREF_END; ++p)
		{
			if (!strncmp (ptr, function[p], strlen (function[p])))
			{
				key = prefToKey (parentKey, p, ptr + strlen (function[p]));
				ksAppendKey (returned, key);
				goto loop_end;
			}
		}
	loop_end:
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

// returns a string representing the preference value depending on
// it's type as a quoted string, integer or boolean value

static char * prefArgToString (const Key * key)
{
	const Key * typeMeta = keyGetMeta (key, "type");
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
	else
	{
		ssize_t len = keyGetValueSize (key) + 2;
		buffer = elektraCalloc (len);
		snprintf (buffer, len, "\"%s\"", keyString (key));
	}
	return buffer;
}

static void writeKey (FILE * fp, const Key * parentKey, const Key * key)
{
	char * prefName = (char *)keyName (key) + strlen (keyName (parentKey)) + 1; // skip parentKey name + '/'
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
	if (!functionName) goto write_cleanup;
	argString = prefArgToString (key);
	if (!argString) goto write_cleanup;
	fprintf (fp, "%s(\"%s\", %s);\n", functionName, realPrefName, argString);
write_cleanup:
	if (realPrefName) elektraFree (realPrefName);
	if (argString) elektraFree (argString);
}

int elektraMozprefsSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	// get all keys
	// this function is optional

	FILE * fp = fopen (keyString (parentKey), "w");
	if (!fp) return -1;
	Key * cur;
	while ((cur = ksNext (returned)) != NULL)
	{
		if (!strcmp (keyName (parentKey), keyName (cur))) continue;
		writeKey (fp, parentKey, cur);
	}
	fclose (fp);
	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (mozprefs)
{
	// clang-format off
	return elektraPluginExport ("mozprefs",
			ELEKTRA_PLUGIN_GET,	&elektraMozprefsGet,
			ELEKTRA_PLUGIN_SET,	&elektraMozprefsSet,
			ELEKTRA_PLUGIN_END);
}

