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

int elektraNiGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	/* get all keys */

	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/ni"))
	{
		ElektraKeyset * moduleConfig =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/ni", ELEKTRA_KEY_VALUE, "ni plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/ni/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/ni/exports/get", ELEKTRA_KEY_FUNC, elektraNiGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/ni/exports/set", ELEKTRA_KEY_FUNC, elektraNiSet, ELEKTRA_KEY_END),
#include "readme_ni.c"
			       elektraKeyNew ("system:/elektra/modules/ni/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, moduleConfig);
		elektraKeysetDel (moduleConfig);
		return 1;
	}

	elektraNi_node root = elektraNi_New ();
	int errnosave = errno;
	int error = elektraNi_ReadFile (root, elektraKeyString (parentKey), 0);
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
		ElektraKey * k = elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_END);
		elektraKeyAddName (k, elektraNi_GetName (current, NULL));
		elektraKeySetString (k, elektraNi_GetValue (current, NULL));
		elektraNi_node mcur = NULL;
		while ((mcur = elektraNi_GetNextChild (current, mcur)) != NULL)
		{
			elektraKeySetMeta (k, elektraNi_GetName (mcur, NULL), elektraNi_GetValue (mcur, NULL));
			// printf("get meta %s %s from %s\n", elektraNi_GetName(mcur, NULL), elektraNi_GetValue (mcur, NULL), keyName(k));
		}
		elektraKeysetAppendKey (returned, k);
	}

	elektraNi_Free (root);

	return 1; /* success */
}

static void keyMetaToNi (elektraNi_node add, ElektraKey * cur)
{
	elektraNi_SetValue (add, elektraKeyString (cur), elektraKeyGetValueSize (cur) - 1);

	const ElektraKey * m;
	elektraKeyRewindMeta (cur);
	while ((m = elektraKeyNextMeta (cur)) != 0)
	{
		// printf("set meta %s %s from %s\n", keyName(m), keyString(m), keyName(cur));
		elektraNi_node madd = elektraNi_GetChild (add, elektraKeyName (m), elektraKeyGetNameSize (m) - 1, 1, 0);
		elektraNi_SetValue (madd, elektraKeyString (m), elektraKeyGetValueSize (m) - 1);
	}
}

int elektraNiSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	/* set all keys */

	elektraNi_node root = elektraNi_New ();

	ElektraKey * cur;
	elektraKeysetRewind (returned);

	if (elektraKeyCmp (elektraKeysetHead (returned), parentKey) == 0)
	{
		// printf ("found parentkey");
		elektraNi_node add = elektraNi_GetChild (root, NULL, 0, 1, 0);
		keyMetaToNi (add, elektraKeysetHead (returned));
		elektraKeysetNext (returned); // do not process parent in loop again
	}

	while ((cur = elektraKeysetNext (returned)) != 0)
	{
		const char * name = elektraKeyGetRelativeName (cur, parentKey);
		elektraNi_node add = elektraNi_GetChild (root, name, strlen (name), 1, 0);
		keyMetaToNi (add, cur);
	}

	int errnosave = errno;
	int error = elektraNi_WriteFile (root, elektraKeyString (parentKey), 0);
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

