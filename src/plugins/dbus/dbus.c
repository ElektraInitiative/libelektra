/**
 * @file
 *
 * @brief Source for dbus plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <internal/config.h>

#include "./dbus.h"

#include <internal/utility/old_helper.h>

int elektraDbusOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
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

int elektraDbusGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/dbus"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/dbus", KEY_VALUE, "dbus plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/dbus/exports", KEY_END),
			keyNew ("system:/elektra/modules/dbus/exports/open", KEY_FUNC, elektraDbusOpen, KEY_END),
			keyNew ("system:/elektra/modules/dbus/exports/get", KEY_FUNC, elektraDbusGet, KEY_END),
			keyNew ("system:/elektra/modules/dbus/exports/commit", KEY_FUNC, elektraDbusCommit, KEY_END),
			keyNew ("system:/elektra/modules/dbus/exports/hook/notification/send/get", KEY_FUNC, elektraDbusGet, KEY_END),
			keyNew ("system:/elektra/modules/dbus/exports/hook/notification/send/set", KEY_FUNC, elektraDbusCommit, KEY_END),
			keyNew ("system:/elektra/modules/dbus/exports/close", KEY_FUNC, elektraDbusClose, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/dbus/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	// remember all keys
	ElektraDbusPluginData * pluginData = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (pluginData);

	KeySet * ks = pluginData->keys;
	if (ks) ksDel (ks);
	pluginData->keys = ksDup (returned);

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
static void announceKeys (KeySet * ks, const char * signalName, DBusBusType busType, ElektraDbusPluginData * data)
{
	ELEKTRA_NOT_NULL (ks);
	ELEKTRA_NOT_NULL (signalName);
	ELEKTRA_NOT_NULL (data);

	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		Key * k = ksAtCursor (ks, it);
		elektraDbusSendMessage (data, busType, keyName (k), signalName);
	}
}

int elektraDbusCommit (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ElektraDbusPluginData * pluginData = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (pluginData);

	// because elektraLogchangeGet will always be executed before elektraLogchangeCommit
	// we know that oldKeys must exist here!
	KeySet * oldKeys = pluginData->keys;

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

	Key * resolvedParentKey = parentKey;
	// Resolve cascaded parent key to get its namespace
	if (!strncmp (keyName (parentKey), "/", 1))
	{
		resolvedParentKey = ksLookup (returned, parentKey, 0);
	}
	int announceSession = 0;
	int announceSystem = 0;
	if (resolvedParentKey != NULL)
	{
		announceSession = !strncmp (keyName (resolvedParentKey), "user", 4);
		announceSystem = !strncmp (keyName (resolvedParentKey), "system", 6);
	}

	if (!strncmp (keyString (ksLookupByName (elektraPluginGetConfig (handle), "/announce", 0)), "once", 4))
	{
		if (announceSession) elektraDbusSendMessage (pluginData, DBUS_BUS_SESSION, keyName (resolvedParentKey), "Commit");
		if (announceSystem) elektraDbusSendMessage (pluginData, DBUS_BUS_SYSTEM, keyName (resolvedParentKey), "Commit");
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


	ksDel (oldKeys);
	ksDel (addedKeys);
	ksDel (changedKeys);
	ksDel (removedKeys);

	// for next invocation of elektraLogchangeCommit, remember our current keyset
	pluginData->keys = ksDup (returned);

	return 1; /* success */
}

int elektraDbusClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	ElektraDbusPluginData * pluginData = elektraPluginGetData (handle);
	if (pluginData == NULL)
	{
		return 1;
	}

	KeySet * ks = pluginData->keys;
	if (ks) ksDel (ks);

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
		ELEKTRA_PLUGIN_COMMIT,	&elektraDbusCommit,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDbusClose,
		ELEKTRA_PLUGIN_END);
}
