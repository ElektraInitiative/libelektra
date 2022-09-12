/**
 * @file
 *
 * @brief A plugin that converts keys to metakeys and vice versa
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "glob.h"

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <fnmatch.h>
#include <kdbhelper.h>

struct GlobFlagMap
{
	char * name;
	short flag;
};

struct GlobFlagMap flagMaps[] = { { "noescape", FNM_NOESCAPE }, { "pathname", FNM_PATHNAME }, { "period", FNM_PERIOD } };

int elektraGlobMatch (ElektraKey * key, const ElektraKey * match, const char * globFlags)
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

static const char * getGlobFlags (ElektraKeyset * keys, ElektraKey * globKey)
{
	ElektraKey * flagKey = keyDup (globKey, ELEKTRA_KEY_CP_ALL);
	keyAddBaseName (flagKey, "flags");
	ElektraKey * flagResult = ksLookup (keys, flagKey, ELEKTRA_KDB_O_NONE);
	keyDel (flagKey);

	if (flagResult)
	{
		return keyString (flagResult);
	}

	return 0;
}

static ElektraKeyset * getGlobKeys (ElektraKey * parentKey, ElektraKeyset * keys, enum GlobDirection direction)
{
	ElektraKeyset * glob = ksNew (0, ELEKTRA_KS_END);
	ElektraKey * k = 0;
	size_t parentsize = keyGetNameSize (parentKey);

	ElektraKey * userGlobConfig = 0;
	ElektraKey * systemGlobConfig = 0;
	ElektraKey * userDirGlobConfig = 0;
	ElektraKey * systemDirGlobConfig = 0;

	userGlobConfig = keyNew ("user:/glob", ELEKTRA_KEY_END);
	systemGlobConfig = keyNew ("system:/glob", ELEKTRA_KEY_END);
	switch (direction)
	{
	case GET:
		userDirGlobConfig = keyNew ("user:/glob/get", ELEKTRA_KEY_END);
		systemDirGlobConfig = keyNew ("system:/glob/get", ELEKTRA_KEY_END);
		break;
	case SET:
		userDirGlobConfig = keyNew ("user:/glob/set", ELEKTRA_KEY_END);
		systemDirGlobConfig = keyNew ("system:/glob/set", ELEKTRA_KEY_END);
		break;
	}

	while ((k = ksNext (keys)) != 0)
	{
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
			ElektraKey * ins = keyDup (k, ELEKTRA_KEY_CP_ALL);
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

static void applyGlob (ElektraKeyset * returned, ElektraKeyset * glob)
{
	ElektraKey * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != 0)
	{
		ElektraKey * match;
		ksRewind (glob);
		while ((match = ksNext (glob)) != 0)
		{
			const ElektraKey * flagKey = keyGetMeta (match, "glob/flags");
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

int elektraGlobOpen (Plugin * handle ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic should be here */
	/* TODO: name of parentKey is not set...*/
	/* So rewriting cannot happen here (is in elektraGlobSet */

	return 1; /* success */
}

int elektraGlobClose (Plugin * handle ELEKTRA_UNUSED, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	ElektraKeyset * keys = elektraPluginGetData (handle);
	ksDel (keys);

	return 1; /* success */
}


int elektraGlobGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/glob"))
	{
		// TODO: improve plugin contract
		ElektraKeyset * config =
#include "contract.h"
			ksAppend (returned, config);
		ksDel (config);
		return 1;
	}

	ElektraKeyset * keys = elektraPluginGetConfig (handle);
	ksRewind (keys);

	ElektraKeyset * glob = getGlobKeys (parentKey, keys, GET);
	applyGlob (returned, glob);

	ksDel (glob);

	return 1; /* success */
}


int elektraGlobSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraKeyset * keys = elektraPluginGetConfig (handle);
	ksRewind (keys);

	ElektraKeyset * glob = getGlobKeys (parentKey, keys, SET);
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

