/**
 * @file
 *
 * @brief A plugin that converts keys to metakeys and vice versa
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./glob.h"

#ifndef HAVE_KDBCONFIG
#include <internal/config.h>
#endif

#include <fnmatch.h>
#include <internal/utility/old_helper.h>

struct GlobFlagMap
{
	char * name;
	short flag;
};

struct GlobFlagMap flagMaps[] = { { "noescape", FNM_NOESCAPE }, { "pathname", FNM_PATHNAME }, { "period", FNM_PERIOD } };

int elektraGlobMatch (Key * key, const Key * match, const char * globFlags)
{
	char * tokenList = elektraStrDup (globFlags);
	char delimiter[] = ",";
	char * flagName = strtok (tokenList, delimiter);

	int flags = 0;
	while (flagName != NULL)
	{
		for (size_t i = 0; i < sizeof (flagMaps) / sizeof (struct GlobFlagMap); i++)
		{
			if (!strcmp (flagName, flagMaps[i].name))
			{
				flags |= flagMaps[i].flag;
			}
		}
		flagName = strtok (NULL, delimiter);
	}

	free (tokenList);

	if (!fnmatch (keyString (match), keyName (key), flags))
	{
		keyCopyAllMeta (key, match);
		return 1;
	}

	return 0;
}

enum GlobDirection
{
	GET,
	SET,
};

static const char * getGlobFlags (KeySet * keys, Key * globKey)
{
	Key * flagKey = keyDup (globKey, KEY_CP_ALL);
	keyAddBaseName (flagKey, "flags");
	Key * flagResult = ksLookup (keys, flagKey, KDB_O_NONE);
	keyDel (flagKey);

	if (flagResult)
	{
		return keyString (flagResult);
	}

	return 0;
}

static KeySet * getGlobKeys (Key * parentKey, KeySet * keys, enum GlobDirection direction)
{
	KeySet * glob = ksNew (0, KS_END);
	Key * k = 0;
	size_t parentsize = keyGetNameSize (parentKey);

	Key * userGlobConfig = 0;
	Key * systemGlobConfig = 0;
	Key * userDirGlobConfig = 0;
	Key * systemDirGlobConfig = 0;

	userGlobConfig = keyNew ("user:/glob", KEY_END);
	systemGlobConfig = keyNew ("system:/glob", KEY_END);
	switch (direction)
	{
	case GET:
		userDirGlobConfig = keyNew ("user:/glob/get", KEY_END);
		systemDirGlobConfig = keyNew ("system:/glob/get", KEY_END);
		break;
	case SET:
		userDirGlobConfig = keyNew ("user:/glob/set", KEY_END);
		systemDirGlobConfig = keyNew ("system:/glob/set", KEY_END);
		break;
	}

	for (elektraCursor it = 0; it < ksGetSize (keys); ++it)
	{
		k = ksAtCursor (keys, it);
		/* use only glob keys for the current direction */
		if (keyIsDirectlyBelow (userGlobConfig, k) || keyIsDirectlyBelow (systemGlobConfig, k) ||
		    keyIsDirectlyBelow (userDirGlobConfig, k) || keyIsDirectlyBelow (systemDirGlobConfig, k))
		{
			keySetMeta (k, "glob/flags", getGlobFlags (keys, k));

			/* Look if we have a string */
			size_t valsize = keyGetValueSize (k);
			if (valsize < 2) continue;

			/* We now know we want that key.
			 Dup it to not change the configuration. */
			Key * ins = keyDup (k, KEY_CP_ALL);
			/* Now look if we want cascading for the key */
			if (keyString (k)[0] == '/')
			{
				char * newstring = elektraMalloc (valsize + parentsize);
				strcpy (newstring, keyName (parentKey));
				strcat (newstring, keyString (k));
				keySetString (ins, newstring);
				elektraFree (newstring);
			}
			ksAppendKey (glob, ins);
		}
	}

	keyDel (userGlobConfig);
	keyDel (systemGlobConfig);
	keyDel (userDirGlobConfig);
	keyDel (systemDirGlobConfig);

	return glob;
}

static void applyGlob (KeySet * returned, KeySet * glob)
{
	Key * cur;

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		cur = ksAtCursor (returned, it);
		Key * match;

		for (elektraCursor itGlob = 0; itGlob < ksGetSize (glob); ++itGlob)
		{
			match = ksAtCursor (glob, itGlob);
			const Key * flagKey = keyGetMeta (match, "glob/flags");
			int matchApplied;

			if (flagKey)
			{
				matchApplied = elektraGlobMatch (cur, match, keyString (flagKey));
			}
			else
			{
				/* if no flags were provided, default to FNM_PATHNAME behaviour */
				matchApplied = elektraGlobMatch (cur, match, "pathname");
			}

			if (matchApplied) break;
		}
	}
}

int elektraGlobOpen (Plugin * handle ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic should be here */
	/* TODO: name of parentKey is not set...*/
	/* So rewriting cannot happen here (is in elektraGlobSet */

	return 1; /* success */
}

int elektraGlobClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	KeySet * keys = elektraPluginGetData (handle);
	ksDel (keys);

	return 1; /* success */
}


int elektraGlobGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/glob"))
	{
		// TODO: improve plugin contract
		KeySet * config =
#include "./contract.h"
			ksAppend (returned, config);
		ksDel (config);
		return 1;
	}

	KeySet * keys = elektraPluginGetConfig (handle);
	KeySet * glob = getGlobKeys (parentKey, keys, GET);

	applyGlob (returned, glob);

	ksDel (glob);

	return 1; /* success */
}


int elektraGlobSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	KeySet * keys = elektraPluginGetConfig (handle);
	KeySet * glob = getGlobKeys (parentKey, keys, SET);

	applyGlob (returned, glob);

	ksDel (glob);

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("glob",
		ELEKTRA_PLUGIN_OPEN,	&elektraGlobOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraGlobClose,
		ELEKTRA_PLUGIN_GET,	&elektraGlobGet,
		ELEKTRA_PLUGIN_SET,	&elektraGlobSet,
		ELEKTRA_PLUGIN_END);
}

