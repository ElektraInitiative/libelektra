/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include "null.h"

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <string.h>
#include <stdlib.h>

int elektraNullGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	if (!strcmp (keyName(parentKey), "system/elektra/modules/null"))
	{
		KeySet *moduleConfig = ksNew (30,
			keyNew ("system/elektra/modules/null",
				KEY_VALUE, "null plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/null/exports", KEY_END),
			keyNew ("system/elektra/modules/null/exports/get",
				KEY_FUNC, elektraNullGet, KEY_END),
			keyNew ("system/elektra/modules/null/exports/set",
				KEY_FUNC, elektraNullSet, KEY_END),
#include "readme_null.c"
			keyNew ("system/elektra/modules/null/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);

		return 1;
	}
	/* get all keys */

	Key *k;
	ksRewind (returned);
	while ((k = ksNext (returned)) != 0)
	{
		if (!strcmp (keyString(k), "@NULL"))
		{
			keySetBinary (k, 0, 0);
		}
		else if (!strcmp (keyString(k), "@EMPTY"))
		{
			keySetString (k, "");
		}
		else if (!strncmp (keyString(k), "@@", 2))
		{
			/* Drop the first of the @ */
			keySetString(k, keyString(k)+1);
		}
	}

	return 1; /* success */
}

int elektraNullSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	Key *k;
	ksRewind (returned);
	while ((k = ksNext (returned)) != 0)
	{
		if (keyValue(k) == 0)
		{
			keySetString (k, "@NULL");
		}
		else if (!strcmp (keyValue(k), ""))
		{
			keySetString (k, "@EMPTY");
		}
		else if (!strncmp (keyValue(k), "@", 1))
		{
			char *n = elektraMalloc (keyGetValueSize(k)+1);
			strcpy (n, "@");
			strcat (n, keyValue(k));
			keySetString (k, n);
			free (n);
		}
	}

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(null)
{
	return elektraPluginExport("null",
		ELEKTRA_PLUGIN_GET,	&elektraNullGet,
		ELEKTRA_PLUGIN_SET,	&elektraNullSet,
		ELEKTRA_PLUGIN_END);
}

