/**
 * @file
 *
 * @brief Source for logchange plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <stdio.h>
#include <string.h>

#include "logchange.h"

static void logKeys (ElektraKeyset * ks, const char * message)
{
	elektraKeysetRewind (ks);
	ElektraKey * k = 0;
	while ((k = elektraKeysetNext (ks)) != 0)
	{
		printf ("%s: %s\n", message, elektraKeyName (k));
	}
}

int elektraLogchangeGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/logchange"))
	{
		ElektraKeyset * contract = elektraKeysetNew (
			30, elektraKeyNew ("system:/elektra/modules/logchange", ELEKTRA_KEY_VALUE, "logchange plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/logchange/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/logchange/exports/get", ELEKTRA_KEY_FUNC, elektraLogchangeGet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/logchange/exports/set", ELEKTRA_KEY_FUNC, elektraLogchangeSet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/logchange/exports/close", ELEKTRA_KEY_FUNC, elektraLogchangeClose, ELEKTRA_KEY_END),
#include ELEKTRA_README
			elektraKeyNew ("system:/elektra/modules/logchange/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; /* success */
	}

	// remember all keys
	ElektraKeyset * ks = (ElektraKeyset *) elektraPluginGetData (handle);
	if (ks) elektraKeysetDel (ks);
	elektraPluginSetData (handle, elektraKeysetDup (returned));

	if (strncmp (elektraKeyString (elektraKeysetLookupByName (elektraPluginGetConfig (handle), "/log/get", 0)), "1", 1) == 0)
	{
		ElektraKeyset * logset = elektraKeysetNew (1, elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL), ELEKTRA_KS_END);
		logKeys (logset, "loading configuration");
		elektraKeysetDel (logset);
	}

	return 1; /* success */
}

int elektraLogchangeSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ElektraKeyset * oldKeys = (ElektraKeyset *) elektraPluginGetData (handle);
	// because elektraLogchangeGet will always be executed before elektraLogchangeSet
	// we know that oldKeys must exist here!
	elektraKeysetRewind (oldKeys);
	elektraKeysetRewind (returned);

	ElektraKeyset * addedKeys = elektraKeysetDup (returned);
	ElektraKeyset * changedKeys = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * removedKeys = elektraKeysetNew (0, ELEKTRA_KS_END);

	ElektraKey * k = 0;
	while ((k = elektraKeysetNext (oldKeys)) != 0)
	{
		ElektraKey * p = elektraKeysetLookup (addedKeys, k, ELEKTRA_KDB_O_POP);
		// Note: keyDel not needed, because at least two references exist
		if (p)
		{
			if (elektraKeyNeedSync (p))
			{
				elektraKeysetAppendKey (changedKeys, p);
			}
		}
		else
		{
			elektraKeysetAppendKey (removedKeys, k);
		}
	}

	logKeys (addedKeys, "added key");
	logKeys (changedKeys, "changed key");
	logKeys (removedKeys, "removed key");

	elektraKeysetDel (oldKeys);
	elektraKeysetDel (addedKeys);
	elektraKeysetDel (changedKeys);
	elektraKeysetDel (removedKeys);

	// for next invocation of elektraLogchangeSet, remember our current keyset
	elektraPluginSetData (handle, elektraKeysetDup (returned));

	return 1; /* success */
}

int elektraLogchangeClose (Plugin * handle, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ElektraKeyset * ks = (ElektraKeyset *) elektraPluginGetData (handle);
	if (ks) elektraKeysetDel (ks);
	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("logchange",
		ELEKTRA_PLUGIN_GET,	&elektraLogchangeGet,
		ELEKTRA_PLUGIN_SET,	&elektraLogchangeSet,
		ELEKTRA_PLUGIN_CLOSE,	&elektraLogchangeClose,
		ELEKTRA_PLUGIN_END);
}
