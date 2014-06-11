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

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

int elektraGlobMatch(Key *key, const Key *match)
{
	if (!fnmatch (keyString(match), keyName(key),
			FNM_PATHNAME))
	{
		keyCopyAllMeta(key, match);
	}
	return 0;
}

enum GlobDirection {
	GET,
	SET,
};

static KeySet* getGlobKeys(Key* parentKey, KeySet* keys, enum GlobDirection direction)
{
	KeySet* glob = ksNew (0);
	Key* k;
	size_t parentsize = keyGetNameSize (parentKey);
	while ((k = ksNext (keys)) != 0)
	{
		/* First look if it is a glob key at all */
		if (strncmp (keyName (k), "system/glob", sizeof("system/glob") - 1)
				&& strncmp (keyName (k), "user/glob", sizeof("user/glob") - 1))
			continue;

		Key *filterIfBelowUser;
		Key *filterIfBelowSystem;

		if (direction == GET)
		{
			filterIfBelowUser = keyNew ("user/glob/set", KEY_END);
			filterIfBelowSystem = keyNew ("system/glob/set", KEY_END);
		}

		if (direction == SET)
		{
			filterIfBelowUser = keyNew ("user/glob/get", KEY_END);
			filterIfBelowSystem = keyNew ("system/glob/get", KEY_END);
		}

		if (keyIsBelow (filterIfBelowUser, k)
				|| keyIsBelow (filterIfBelowSystem, k))
		{
			keyDel (filterIfBelowUser);
			keyDel (filterIfBelowSystem);
			continue;
		}

		keyDel (filterIfBelowUser);
		keyDel (filterIfBelowSystem);

		/* Look if we have a string */
		size_t valsize = keyGetValueSize (k);
		if (valsize < 2) continue;

		/* We now know we want that key.
		 Dup it to not change the configuration. */
		Key* ins = keyDup (k);
		/* Now look if we want cascading for the key */
		if (keyString (k)[0] == '/')
		{
			char* newstring = malloc (valsize + parentsize);
			strcpy (newstring, keyName (parentKey));
			strcat (newstring, keyString (k));
			keySetString (ins, newstring);
			free (newstring);
		}
		ksAppendKey (glob, ins);
	}
	return glob;
}

static void applyGlob(KeySet* returned, KeySet* glob)
{
	Key* cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != 0)
	{
		Key* match;
		ksRewind (glob);
		while ((match = ksNext (glob)) != 0)
		{
			elektraGlobMatch (cur, match);
		}
	}
}

int elektraGlobOpen(Plugin *handle ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic should be here */
	/* TODO: name of parentKey is not set...*/
	/* So rewriting cannot happen here (is in elektraGlobSet */

	return 1; /* success */
}

int elektraGlobClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	KeySet *keys = elektraPluginGetData(handle);
	ksDel (keys);

	return 1; /* success */
}



int elektraGlobGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName(parentKey), "system/elektra/modules/glob"))
	{
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
				KEY_VALUE, "apply", KEY_END),
			keyNew ("system/elektra/modules/glob/infos/placements",
				KEY_VALUE, "presetstorage postgetstorage", KEY_END),
			keyNew ("system/elektra/modules/glob/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/glob/infos/ordering",
				KEY_VALUE, "check", KEY_END),
			keyNew ("system/elektra/modules/glob/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END));
		ksDel (n);
	}

	KeySet *keys = elektraPluginGetConfig(handle);
	ksRewind (keys);

	KeySet* glob = getGlobKeys (parentKey, keys, GET);
	applyGlob (returned, glob);

	ksDel (glob);

	return 1; /* success */
}



int elektraGlobSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	KeySet *keys = elektraPluginGetConfig(handle);
	ksRewind (keys);

	KeySet* glob = getGlobKeys (parentKey, keys, SET);
	applyGlob (returned, glob);

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

