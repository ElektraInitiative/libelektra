/**
 * @file
 *
 * @brief Source for dpkg plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "dpkg.h"
#include <elektra/old_kdb.h>
#include <internal/utility/old_helper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DPKG_LINE_MAX 512

static void appendToKey (Key * key, const char * line)
{
	char * buffer;
	size_t len = keyGetValueSize (key) + elektraStrLen (line) - 1;
	buffer = elektraMalloc (len);
	snprintf (buffer, len, "%s%s", keyString (key), line);
	keySetString (key, buffer);
	elektraFree (buffer);
}
static KeySet * nextPackage (FILE * fp, Key * parentKey)
{
	char * line = elektraMalloc (DPKG_LINE_MAX);
	KeySet * package = ksNew (500, KS_END);
	Key * lastKey = NULL;
	Key * baseKey = NULL;
	int notDone = 0;
	while (fgets (line, DPKG_LINE_MAX, fp) != NULL)
	{
		if (*line == '\n') break;
		if (*line == ' ')
		{
			if (strchr (line, '\n'))
				notDone = 0;
			else
				notDone = 1;
			appendToKey (lastKey, line);
		}
		else if (notDone)
		{
			if (strchr (line, '\n')) notDone = 0;
			appendToKey (lastKey, line);
		}
		else
		{
			if (!strchr (line, '\n')) notDone = 1;
			char * section = line;
			char * data = strchr (line, ':');
			if (data) *data = '\0';
			++data;		     // skip :
			++data;		     // skip whitespace
			strtok (data, "\n"); // remove newline
			if (!strcmp (section, "Package"))
			{
				baseKey = keyDup (parentKey, KEY_CP_ALL);
				keyAddBaseName (baseKey, data);
				lastKey = baseKey;
				ksAppendKey (package, baseKey);
			}
			else
			{
				Key * key = keyDup (baseKey, KEY_CP_ALL);
				keyAddBaseName (key, section);
				keySetString (key, data);
				lastKey = key;
				ksAppendKey (package, key);
			}
		}
		memset (line, 0, DPKG_LINE_MAX);
	}
	elektraFree (line);
	return package;
}
static KeySet * readFile (Key * parentKey)
{
	FILE * fp = fopen (keyString (parentKey), "r");
	KeySet * result = ksNew (0, KS_END);
	if (!fp) return result;
	while (!feof (fp))
	{
		KeySet * package = nextPackage (fp, parentKey);
		ksAppend (result, package);
		ksDel (package);
	}
	fclose (fp);
	return result;
}
int elektraDpkgGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/dpkg"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/dpkg", KEY_VALUE, "dpkg plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/dpkg/exports", KEY_END),
			       keyNew ("system:/elektra/modules/dpkg/exports/get", KEY_FUNC, elektraDpkgGet, KEY_END),
			       keyNew ("system:/elektra/modules/dpkg/exports/set", KEY_FUNC, elektraDpkgSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/dpkg/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	KeySet * ks = readFile (parentKey);
	ksAppend (returned, ks);
	ksDel (ks);
	return 1; // success
}

int elektraDpkgSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys

	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("dpkg", ELEKTRA_PLUGIN_GET, &elektraDpkgGet, ELEKTRA_PLUGIN_SET, &elektraDpkgSet, ELEKTRA_PLUGIN_END);
}
