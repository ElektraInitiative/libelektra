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

typedef enum
{
	PREF = 0,
	USER,
	LOCK,
	STICKY,
	PREF_END,
} PrefType;


const char * function[] = { "pref", "user_pref", "lockPref", "sticky_pref" };
const char * prefix[] = { "pref", "user", "lock", "sticky" };

static ElektraKey * prefToKey (ElektraKey * parentKey, PrefType type, const char * pref)
{
	ElektraKey * key = elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_END);
	elektraKeyAddBaseName (key, prefix[type]);
	char * localString = elektraStrDup (pref);
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
	if (tPtr) elektraKeyAddBaseName (key, tPtr);
	while ((tPtr = strtok (NULL, ".")) != NULL)
	{
		elektraKeyAddBaseName (key, tPtr);
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
		elektraKeySetMeta (key, "type", "boolean");
		elektraKeySetString (key, prefArg);
	}
	else if (prefArg[0] == '"' && prefArg[strlen (prefArg) - 1] == '"')
	{
		// TODO: else if list
		elektraKeySetMeta (key, "type", "string");
		*prefArg = '\0';
		*(prefArg + (strlen (prefArg + 1))) = '\0';
		elektraKeySetString (key, (prefArg + 1));
	}
	else
	{
		elektraKeySetMeta (key, "type", "integer");
		elektraKeySetString (key, prefArg);
	}
	elektraFree (prefArg);
	elektraFree (localString);
	return key;
}


int elektraMozprefsGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/mozprefs"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/mozprefs", ELEKTRA_KEY_VALUE, "mozprefs plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/mozprefs/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/mozprefs/exports/get", ELEKTRA_KEY_FUNC, elektraMozprefsGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/mozprefs/exports/set", ELEKTRA_KEY_FUNC, elektraMozprefsSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/mozprefs/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; // success
	}
	// get all keys
	const char * fileName = elektraKeyString (parentKey);
	FILE * fp = fopen (fileName, "r");
	int len = 1024;
	char * buffer = elektraMalloc (len * sizeof (char));
	ElektraKey * key;

	while (fgets (buffer, len, fp))
	{
		if (buffer[strlen (buffer) - 1] != '\n')
		{
			fseek (fp, ((len - 1) * (-1)), SEEK_CUR);
			len *= 2;
			elektraRealloc ((void **) &buffer, len * sizeof (char));
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
				elektraKeysetAppendKey (returned, key);
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
	char * src = (char *) prefName;
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

static char * prefArgToString (const ElektraKey * key)
{
	const ElektraKey * typeMeta = elektraKeyGetMeta (key, "type");
	char * buffer = NULL;
	if (!strcmp (elektraKeyString (typeMeta), "boolean"))
	{
		buffer = elektraStrDup (elektraKeyString (key));
	}
	else if (!strcmp (elektraKeyString (typeMeta), "string"))
	{
		ssize_t len = elektraKeyGetValueSize (key) + 2; // size of string + leading and trailing '"'
		buffer = elektraCalloc (len);
		snprintf (buffer, len, "\"%s\"", elektraKeyString (key));
	}
	else if (!strcmp (elektraKeyString (typeMeta), "integer"))
	{
		buffer = elektraStrDup (elektraKeyString (key));
	}
	else
	{
		ssize_t len = elektraKeyGetValueSize (key) + 2;
		buffer = elektraCalloc (len);
		snprintf (buffer, len, "\"%s\"", elektraKeyString (key));
	}
	return buffer;
}

static void writeKey (FILE * fp, const ElektraKey * parentKey, const ElektraKey * key)
{
	char * prefName = (char *) elektraKeyName (key) + strlen (elektraKeyName (parentKey)) + 1; // skip parentKey name + '/'
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

int elektraMozprefsSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	// get all keys
	// this function is optional

	FILE * fp = fopen (elektraKeyString (parentKey), "w");
	if (!fp) return -1;
	ElektraKey * cur;
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		if (!strcmp (elektraKeyName (parentKey), elektraKeyName (cur))) continue;
		writeKey (fp, parentKey, cur);
	}
	fclose (fp);
	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("mozprefs",
			ELEKTRA_PLUGIN_GET,	&elektraMozprefsGet,
			ELEKTRA_PLUGIN_SET,	&elektraMozprefsSet,
			ELEKTRA_PLUGIN_END);
}

