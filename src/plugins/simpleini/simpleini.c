/***************************************************************************
                     simpleini.c  -  Skeleton of a plugin
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "simpleini.h"

int elektraSimpleiniGet(Plugin *handle, KeySet *returned, Key *parentKey)
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
			keyNew ("system/elektra/modules/simpleini/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/simpleini/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/simpleini/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/simpleini/infos/description",
				KEY_VALUE, "Very simple storage which writes out in a basic ini format", KEY_END),
			keyNew ("system/elektra/modules/simpleini/infos/provides",
				KEY_VALUE, "storage", KEY_END),
			keyNew ("system/elektra/modules/simpleini/infos/placements",
				KEY_VALUE, "getstorage setstorage", KEY_END),
			keyNew ("system/elektra/modules/simpleini/infos/needs",
				KEY_VALUE, "code", KEY_END),
			keyNew ("system/elektra/modules/simpleini/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	int n;
	FILE *fp = fopen (keyString(parentKey), "r");
	char key[1000];
	char value[1000];

	while ((n = fscanf (fp, "%999s = %999s\n", key, value)) >= 1)
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

int elektraSimpleiniSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* set all keys */

	FILE *fp = fopen(keyString(parentKey), "w");

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

