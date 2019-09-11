/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "ni.h"

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <kdbease.h>
#include <kdberrors.h>

#include <errno.h>
#include <string.h>

int elektraNiGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	/* get all keys */

	if (!strcmp (keyName (parentKey), "system/elektra/modules/ni"))
	{
		KeySet * moduleConfig =
			ksNew (30, keyNew ("system/elektra/modules/ni", KEY_VALUE, "ni plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/ni/exports", KEY_END),
			       keyNew ("system/elektra/modules/ni/exports/get", KEY_FUNC, elektraNiGet, KEY_END),
			       keyNew ("system/elektra/modules/ni/exports/set", KEY_FUNC, elektraNiSet, KEY_END),
#include "readme_ni.c"
			       keyNew ("system/elektra/modules/ni/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	elektraNi_node root = elektraNi_New ();
	int errnosave = errno;
	int error = elektraNi_ReadFile (root, keyString (parentKey), 0);
	if (error == 0)
	{
		elektraNi_Free (root);
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}

	elektraNi_node current = NULL;
	while ((current = elektraNi_GetNextChild (root, current)) != NULL)
	{
		Key * k = keyNew (keyName (parentKey), KEY_END);
		keyAddName (k, elektraNi_GetName (current, NULL));
		keySetString (k, elektraNi_GetValue (current, NULL));
		elektraNi_node mcur = NULL;
		while ((mcur = elektraNi_GetNextChild (current, mcur)) != NULL)
		{
			keySetMeta (k, elektraNi_GetName (mcur, NULL), elektraNi_GetValue (mcur, NULL));
			// printf("get meta %s %s from %s\n", elektraNi_GetName(mcur, NULL), elektraNi_GetValue (mcur, NULL), keyName(k));
		}
		ksAppendKey (returned, k);
	}

	elektraNi_Free (root);

	return 1; /* success */
}

static void keyMetaToNi (elektraNi_node add, Key * cur)
{
	elektraNi_SetValue (add, keyString (cur), keyGetValueSize (cur) - 1);

	const Key * m;
	keyRewindMeta (cur);
	while ((m = keyNextMeta (cur)) != 0)
	{
		// printf("set meta %s %s from %s\n", keyName(m), keyString(m), keyName(cur));
		elektraNi_node madd = elektraNi_GetChild (add, keyName (m), keyGetNameSize (m) - 1, 1, 0);
		elektraNi_SetValue (madd, keyString (m), keyGetValueSize (m) - 1);
	}
}

int elektraNiSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	/* set all keys */

	elektraNi_node root = elektraNi_New ();

	Key * cur;
	ksRewind (returned);

	if (keyCmp (ksHead (returned), parentKey) == 0)
	{
		// printf ("found parentkey");
		elektraNi_node add = elektraNi_GetChild (root, NULL, 0, 1, 0);
		keyMetaToNi (add, ksHead (returned));
		ksNext (returned); // do not process parent in loop again
	}

	while ((cur = ksNext (returned)) != 0)
	{
		const char * name = elektraKeyGetRelativeName (cur, parentKey);
		elektraNi_node add = elektraNi_GetChild (root, name, strlen (name), 1, 0);
		keyMetaToNi (add, cur);
	}

	int errnosave = errno;
	int error = elektraNi_WriteFile (root, keyString (parentKey), 0);
	elektraNi_Free (root);

	if (error == 0)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return -1;
	}

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("ni",
		ELEKTRA_PLUGIN_GET,	&elektraNiGet,
		ELEKTRA_PLUGIN_SET,	&elektraNiSet,
		ELEKTRA_PLUGIN_END);
}

