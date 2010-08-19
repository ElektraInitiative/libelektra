/***************************************************************************
                     glob.c  -  Skeleton of a plugin
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


#include "glob.h"

int elektraGlobMatch(Key *key, const Key *match)
{
	if (!fnmatch (keyString(match), keyName(key),
			FNM_PATHNAME))
	{
		keyCopyAllMeta(key, match);
	}
	return 0;
}

int elektraGlobOpen(Plugin *handle, Key *parentKey)
{
	/* plugin initialization logic should be here */
	/* TODO: name of parentKey is not set...*/
	/* So rewriting cannot happen here (is in elektraGlobSet */

	return 1; /* success */
}

int elektraGlobClose(Plugin *handle, Key *errorKey)
{
	/* free all plugin resources and shut it down */

	KeySet *keys = elektraPluginGetData(handle);
	ksDel (keys);

	return 1; /* success */
}

int elektraGlobGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* configuration only */
	KeySet *n;
	ksAppend (returned, n=ksNew (30,
		keyNew ("system/elektra/modules/glob",
			KEY_VALUE, "glob plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/glob/exports", KEY_END),
		keyNew ("system/elektra/modules/glob/exports/open",
			KEY_FUNC, elektraGlobOpen,
			KEY_END),
		keyNew ("system/elektra/modules/glob/exports/close",
			KEY_FUNC, elektraGlobClose,
			KEY_END),
		keyNew ("system/elektra/modules/glob/exports/get",
			KEY_FUNC, elektraGlobGet,
			KEY_END),
		keyNew ("system/elektra/modules/glob/exports/set",
			KEY_FUNC, elektraGlobSet,
			KEY_END),
		keyNew ("system/elektra/modules/glob/exports/elektraGlobMatch",
			KEY_FUNC, elektraGlobMatch,
			KEY_END),
		keyNew ("system/elektra/modules/glob/infos",
			KEY_VALUE, "All information you want to know", KEY_END),
		keyNew ("system/elektra/modules/glob/infos/author",
			KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
		keyNew ("system/elektra/modules/glob/infos/licence",
			KEY_VALUE, "BSD", KEY_END),
		keyNew ("system/elektra/modules/glob/infos/description",
			KEY_VALUE, "Copies meta data to keys using globbing", KEY_END),
		keyNew ("system/elektra/modules/glob/infos/provides",
			KEY_VALUE, "glob", KEY_END),
		keyNew ("system/elektra/modules/glob/infos/placements",
			KEY_VALUE, "presetstorage", KEY_END),
		keyNew ("system/elektra/modules/glob/infos/needs",
			KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/modules/glob/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraGlobSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	KeySet *keys = elektraPluginGetConfig(handle);
	KeySet *glob = ksNew (0);

	Key *k;
	ksRewind (keys);

	size_t parentsize = keyGetNameSize(parentKey);
	while ((k = ksNext(keys)) != 0)
	{

		/* First look if it is a glob key at all */
		if (strncmp (keyName(k), "system/glob", sizeof("system/glob")-1) &&
		    strncmp (keyName(k), "user/glob", sizeof("user/glob")-1)) continue;

		/* Look if we have a string */
		size_t valsize = keyGetValueSize(k);
		if (valsize < 2) continue;

		/* We now know we want that key.
		   Dup it to not change the configuration. */
		Key *ins = keyDup (k);

		/* Now look if we want cascading for the key */
		if (keyString(k)[0] == '/')
		{
			char *newstring = malloc (valsize + parentsize);
			strcpy (newstring, keyName(parentKey));
			strcat (newstring, keyString(k));

			keySetString (ins, newstring);
			free (newstring);
		}

		ksAppendKey (glob, ins);
	}

	Key *cur;
	ksRewind (returned);
	while ((cur = ksNext(returned)) != 0)
	{
		Key *match;
		ksRewind (glob);
		while ((match = ksNext(glob)) != 0)
		{
			elektraGlobMatch (cur, match);
		}
	}

	ksDel (glob);

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(glob)
{
	return elektraPluginExport("glob",
		ELEKTRA_PLUGIN_OPEN,	&elektraGlobOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraGlobClose,
		ELEKTRA_PLUGIN_GET,	&elektraGlobGet,
		ELEKTRA_PLUGIN_SET,	&elektraGlobSet,
		ELEKTRA_PLUGIN_END);
}

