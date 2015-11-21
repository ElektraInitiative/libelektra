/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include "simpleini.h"
#include <errno.h>

#include <kdberrors.h>

#include <stdio.h>
#include <stdlib.h>


int elektraSimpleiniGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	/* get all keys */

	if (!strcmp (keyName(parentKey), "system/elektra/modules/simpleini"))
	{
		KeySet *moduleConfig = ksNew (30,
			keyNew ("system/elektra/modules/simpleini",
				KEY_VALUE, "simpleini plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/simpleini/exports", KEY_END),
			keyNew ("system/elektra/modules/simpleini/exports/get",
				KEY_FUNC, elektraSimpleiniGet, KEY_END),
			keyNew ("system/elektra/modules/simpleini/exports/set",
				KEY_FUNC, elektraSimpleiniSet, KEY_END),
#include "readme_simpleini.c"
			keyNew ("system/elektra/modules/simpleini/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			keyNew ("system/elektra/modules/simpleini/config/needs",
				KEY_VALUE, "the needed configuration to work in a backend", KEY_END),
			keyNew ("system/elektra/modules/simpleini/config/needs/chars",
				KEY_VALUE, "Characters needed", KEY_END),
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/20",
				KEY_VALUE, "61", KEY_END), // space -> a
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/23",
				KEY_VALUE, "62", KEY_END), // # -> b
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/25",
				KEY_VALUE, "63", KEY_END), // % -> c (escape character)
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/3B",
				KEY_VALUE, "64", KEY_END), // ; -> d
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/3D",
				KEY_VALUE, "65", KEY_END), // = -> e
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/5C",
				KEY_VALUE, "66", KEY_END), // \\ -> f
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/0A",
				KEY_VALUE, "67", KEY_END), // enter (NL) -> g
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/0D",
				KEY_VALUE, "68", KEY_END), // CR -> h
			keyNew ("system/elektra/modules/simpleini/config/needs/escape",
				KEY_VALUE, "25", KEY_END),
			KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	char *key;
	char *value;
	int errnosave = errno;
	FILE *fp = fopen (keyString(parentKey), "r");
	if (!fp)
	{
		ELEKTRA_SET_ERROR_GET(parentKey);
		errno = errnosave;
		return -1;
	}


	int n;
#pragma GCC diagnostic ignored "-Wformat"
	// icc warning #269: invalid format string conversion
	while ((n = fscanf (fp, "%ms = %ms\n", &key, &value)) >= 1)
	{
		Key *read = keyNew(0);
		if (keySetName(read, key) == -1)
		{
			fclose (fp);
			keyDel (read);
			ELEKTRA_SET_ERROR(59, parentKey, key);
			return -1;
		}
		keySetString(read, value);

		ksAppendKey (returned, read);
		free (key);
		free (value);
	}

	if (feof(fp) == 0)
	{
		fclose (fp);
		ELEKTRA_SET_ERROR(60, parentKey, "not at the end of file");
		return -1;
	}

	fclose (fp);

	return 1; /* success */
}

int elektraSimpleiniSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	/* set all keys */

	FILE *fp = fopen(keyString(parentKey), "w");
	if (!fp)
	{
		ELEKTRA_SET_ERROR(74, parentKey, keyString(parentKey));
		return -1;
	}

	Key *cur;
	ksRewind (returned);
	while ((cur = ksNext(returned)) != 0)
	{
		fprintf (fp, "%s = %s\n", keyName(cur), keyString(cur));
	}

	fclose (fp);

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(simpleini)
{
	return elektraPluginExport("simpleini",
		ELEKTRA_PLUGIN_GET,	&elektraSimpleiniGet,
		ELEKTRA_PLUGIN_SET,	&elektraSimpleiniSet,
		ELEKTRA_PLUGIN_END);
}

