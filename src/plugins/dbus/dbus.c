/**
 * @file
 *
 * @brief Source for dbus plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "dbus.h"

#include <kdbhelper.h>

int elektraDbusOpen (Plugin * handle, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	ElektraDbusPluginData * data = elektraPluginGetData (handle);

	if (!data)
	{
		data = elektraMalloc (sizeof (*data));
		data->keys = NULL;
		data->systemBus = NULL;
		data->sessionBus = NULL;
	}
	elektraPluginSetData (handle, data);

	return 1; /* success */
}

int elektraDbusGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/dbus"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/dbus", ELEKTRA_KEY_VALUE, "dbus plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/dbus/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/dbus/exports/open", ELEKTRA_KEY_FUNC, elektraDbusOpen, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/dbus/exports/get", ELEKTRA_KEY_FUNC, elektraDbusGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/dbus/exports/set", ELEKTRA_KEY_FUNC, elektraDbusSet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/dbus/exports/close", ELEKTRA_KEY_FUNC, elektraDbusClose, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/dbus/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; /* success */
	}

	// remember all keys
	ElektraDbusPluginData * pluginData = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (pluginData);

	ElektraKeyset * ks = pluginData->keys;
	if (ks) elektraKeysetDel (ks);
	pluginData->keys = elektraKeysetDup (returned);

	return 1; /* success */
}

/**
 * @internal
 * Announce multiple keys with same signal name.
 *
 * @param ks         key set containing modified keys
 * @param signalName signal name to use
 * @param busType    D-Bus bus type
 * @param data       plugin data containing D-Bus connections, etc.
 */
static void announceKeys (ElektraKeyset * ks, const char * signalName, DBusBusType busType, ElektraDbusPluginData * data)
{
	ELEKTRA_NOT_NULL (ks);
	ELEKTRA_NOT_NULL (signalName);
	ELEKTRA_NOT_NULL (data);

	elektraKeysetRewind (ks);
	ElektraKey * k = 0;
	while ((k = elektraKeysetNext (ks)) != 0)
	{
		elektraDbusSendMessage (data, busType, elektraKeyName (k), signalName);
	}
}

int elektraDbusSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraDbusPluginData * pluginData = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (pluginData);

	ElektraKeyset * oldKeys = pluginData->keys;
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

	ElektraKey * resolvedParentKey = parentKey;
	// Resolve cascaded parent key to get its namespace
	if (!strncmp (elektraKeyName (parentKey), "/", 1))
	{
		resolvedParentKey = elektraKeysetLookup (returned, parentKey, 0);
	}
	int announceSession = 0;
	int announceSystem = 0;
	if (resolvedParentKey != NULL)
	{
		announceSession = !strncmp (elektraKeyName (resolvedParentKey), "user", 4);
		announceSystem = !strncmp (elektraKeyName (resolvedParentKey), "system", 6);
	}

	if (!strncmp (elektraKeyString (elektraKeysetLookupByName (elektraPluginGetConfig (handle), "/announce", 0)), "once", 4))
	{
		if (announceSession) elektraDbusSendMessage (pluginData, DBUS_BUS_SESSION, elektraKeyName (resolvedParentKey), "Commit");
		if (announceSystem) elektraDbusSendMessage (pluginData, DBUS_BUS_SYSTEM, elektraKeyName (resolvedParentKey), "Commit");
	}
	else
	{
		if (announceSession)
		{
			announceKeys (addedKeys, "KeyAdded", DBUS_BUS_SESSION, pluginData);
			announceKeys (changedKeys, "KeyChanged", DBUS_BUS_SESSION, pluginData);
			announceKeys (removedKeys, "KeyDeleted", DBUS_BUS_SESSION, pluginData);
		}
		if (announceSystem)
		{
			announceKeys (addedKeys, "KeyAdded", DBUS_BUS_SYSTEM, pluginData);
			announceKeys (changedKeys, "KeyChanged", DBUS_BUS_SYSTEM, pluginData);
			announceKeys (removedKeys, "KeyDeleted", DBUS_BUS_SYSTEM, pluginData);
		}
	}


	elektraKeysetDel (oldKeys);
	elektraKeysetDel (addedKeys);
	elektraKeysetDel (changedKeys);
	elektraKeysetDel (removedKeys);

	// for next invocation of elektraLogchangeSet, remember our current keyset
	pluginData->keys = elektraKeysetDup (returned);

	return 1; /* success */
}

int elektraDbusClose (Plugin * handle, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ElektraDbusPluginData * pluginData = elektraPluginGetData (handle);
	if (pluginData == NULL)
	{
		return 1;
	}

	ElektraKeyset * ks = pluginData->keys;
	if (ks) elektraKeysetDel (ks);

	if (pluginData->systemBus)
	{
		dbus_connection_unref (pluginData->systemBus);
		pluginData->systemBus = NULL;
	}
	if (pluginData->sessionBus)
	{
		dbus_connection_unref (pluginData->sessionBus);
		pluginData->sessionBus = NULL;
	}

	elektraFree (pluginData);
	elektraPluginSetData (handle, NULL);

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("dbus",
		ELEKTRA_PLUGIN_OPEN,	&elektraDbusOpen,
		ELEKTRA_PLUGIN_GET,	&elektraDbusGet,
		ELEKTRA_PLUGIN_SET,	&elektraDbusSet,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDbusClose,
		ELEKTRA_PLUGIN_END);
}
