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

static void logKeys (KeySet * ks, const char * message)
{
	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		Key * k = ksAtCursor (ks, it);
		printf ("%s: %s\n", message, keyName (k));
	}
}

int elektraLogchangeGet (Plugin * handle, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/logchange"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/logchange", KEY_VALUE, "logchange plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/logchange/exports", KEY_END),
			keyNew ("system:/elektra/modules/logchange/exports/get", KEY_FUNC, elektraLogchangeGet, KEY_END),
			keyNew ("system:/elektra/modules/logchange/exports/commit", KEY_FUNC, elektraLogchangeCommit, KEY_END),
			keyNew ("system:/elektra/modules/logchange/exports/hook/notification/send/get", KEY_FUNC, elektraLogchangeGet,
				KEY_END),
			keyNew ("system:/elektra/modules/logchange/exports/hook/notification/send/set", KEY_FUNC, elektraLogchangeCommit,
				KEY_END),
			keyNew ("system:/elektra/modules/logchange/exports/close", KEY_FUNC, elektraLogchangeClose, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/logchange/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	// remember all keys
	KeySet * ks = (KeySet *) elektraPluginGetData (handle);
	if (ks) ksDel (ks);
	elektraPluginSetData (handle, ksDup (returned));

	if (strncmp (keyString (ksLookupByName (elektraPluginGetConfig (handle), "/log/get", 0)), "1", 1) == 0)
	{
		KeySet * logset = ksNew (1, keyDup (parentKey, KEY_CP_ALL), KS_END);
		logKeys (logset, "loading configuration");
		ksDel (logset);
	}

	return 1; /* success */
}

int elektraLogchangeCommit (Plugin * handle, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	// because elektraLogchangeGet will always be executed before elektraLogchangeCommit
	// we know that oldKeys must exist here!
	KeySet * oldKeys = (KeySet *) elektraPluginGetData (handle);
	KeySet * addedKeys = ksDup (returned);
	KeySet * changedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);

	for (elektraCursor it = 0; it < ksGetSize (oldKeys); ++it)
	{
		Key * k = ksAtCursor (oldKeys, it);
		Key * p = ksLookup (addedKeys, k, KDB_O_POP);
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

	// for next invocation of elektraLogchangeCommit, remember our current keyset
	elektraPluginSetData (handle, ksDup (returned));

	return 1; /* success */
}

int elektraLogchangeClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	KeySet * ks = (KeySet *) elektraPluginGetData (handle);
	if (ks) ksDel (ks);
	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("logchange",
		ELEKTRA_PLUGIN_GET,	&elektraLogchangeGet,
		ELEKTRA_PLUGIN_COMMIT,	&elektraLogchangeCommit,
		ELEKTRA_PLUGIN_CLOSE,	&elektraLogchangeClose,
		ELEKTRA_PLUGIN_END);
}
