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
	ksRewind (ks);
	ElektraKey * k = 0;
	while ((k = ksNext (ks)) != 0)
	{
		printf ("%s: %s\n", message, keyName (k));
	}
}

int elektraLogchangeGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/logchange"))
	{
		ElektraKeyset * contract = ksNew (
			30, keyNew ("system:/elektra/modules/logchange", ELEKTRA_KEY_VALUE, "logchange plugin waits for your orders", ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/logchange/exports", ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/logchange/exports/get", ELEKTRA_KEY_FUNC, elektraLogchangeGet, ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/logchange/exports/set", ELEKTRA_KEY_FUNC, elektraLogchangeSet, ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/logchange/exports/close", ELEKTRA_KEY_FUNC, elektraLogchangeClose, ELEKTRA_KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/logchange/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	// remember all keys
	ElektraKeyset * ks = (ElektraKeyset *) elektraPluginGetData (handle);
	if (ks) ksDel (ks);
	elektraPluginSetData (handle, ksDup (returned));

	if (strncmp (keyString (ksLookupByName (elektraPluginGetConfig (handle), "/log/get", 0)), "1", 1) == 0)
	{
		ElektraKeyset * logset = ksNew (1, keyDup (parentKey, ELEKTRA_KEY_CP_ALL), ELEKTRA_KS_END);
		logKeys (logset, "loading configuration");
		ksDel (logset);
	}

	return 1; /* success */
}

int elektraLogchangeSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ElektraKeyset * oldKeys = (ElektraKeyset *) elektraPluginGetData (handle);
	// because elektraLogchangeGet will always be executed before elektraLogchangeSet
	// we know that oldKeys must exist here!
	ksRewind (oldKeys);
	ksRewind (returned);

	ElektraKeyset * addedKeys = ksDup (returned);
	ElektraKeyset * changedKeys = ksNew (0, ELEKTRA_KS_END);
	ElektraKeyset * removedKeys = ksNew (0, ELEKTRA_KS_END);

	ElektraKey * k = 0;
	while ((k = ksNext (oldKeys)) != 0)
	{
		ElektraKey * p = ksLookup (addedKeys, k, ELEKTRA_KDB_O_POP);
		// Note: keyDel not needed, because at least two references exist
		if (p)
		{
			if (keyNeedSync (p))
			{
				ksAppendKey (changedKeys, p);
			}
		}
		else
		{
			ksAppendKey (removedKeys, k);
		}
	}

	logKeys (addedKeys, "added key");
	logKeys (changedKeys, "changed key");
	logKeys (removedKeys, "removed key");

	ksDel (oldKeys);
	ksDel (addedKeys);
	ksDel (changedKeys);
	ksDel (removedKeys);

	// for next invocation of elektraLogchangeSet, remember our current keyset
	elektraPluginSetData (handle, ksDup (returned));

	return 1; /* success */
}

int elektraLogchangeClose (Plugin * handle, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ElektraKeyset * ks = (ElektraKeyset *) elektraPluginGetData (handle);
	if (ks) ksDel (ks);
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
