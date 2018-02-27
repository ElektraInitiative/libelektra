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

void elektraDbusSetIoBinding (Plugin * handle, ElektraIoInterface * binding)
{
	ElektraDbusPluginData * data = elektraPluginGetData (handle);
	data->ioBinding = binding;
}

int elektraDbusOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	ElektraDbusPluginData * data = (ElektraDbusPluginData *) elektraPluginGetData (handle);
	if (!data)
	{
		data = elektraMalloc (sizeof (*data));
		data->keys = NULL;
		data->ioBinding = NULL;
		data->systemBus = NULL;
		data->systemBusAdapter = NULL;
		data->sessionBus = NULL;
		data->sessionBusAdapter = NULL;
	}
	elektraPluginSetData (handle, data);

	return 1; /* success */
}

int elektraDbusGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/dbus"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/dbus", KEY_VALUE, "dbus plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/dbus/exports", KEY_END),
			       keyNew ("system/elektra/modules/dbus/exports/open", KEY_FUNC, elektraDbusOpen, KEY_END),
			       keyNew ("system/elektra/modules/dbus/exports/get", KEY_FUNC, elektraDbusGet, KEY_END),
			       keyNew ("system/elektra/modules/dbus/exports/set", KEY_FUNC, elektraDbusSet, KEY_END),
			       keyNew ("system/elektra/modules/dbus/exports/close", KEY_FUNC, elektraDbusClose, KEY_END),
			       keyNew ("system/elektra/modules/dbus/exports/setIoBinding", KEY_FUNC, elektraDbusSetIoBinding, KEY_END),
#include ELEKTRA_README (dbus)
			       keyNew ("system/elektra/modules/dbus/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	// remember all keys
	ElektraDbusPluginData * pluginData = elektraPluginGetData (handle);
	KeySet * ks = pluginData->keys;
	if (ks) ksDel (ks);
	pluginData->keys = ksDup (returned);

	return 1; /* success */
}

static void announceKeys (KeySet * ks, const char * signalName, DBusBusType busType, ElektraDbusPluginData * data)
{
	ksRewind (ks);
	Key * k = 0;
	while ((k = ksNext (ks)) != 0)
	{
		elektraDbusSendMessage (data, busType, keyName (k), signalName);
	}
}

int elektraDbusSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ElektraDbusPluginData * pluginData = elektraPluginGetData (handle);
	KeySet * oldKeys = pluginData->keys;
	// because elektraLogchangeGet will always be executed before elektraLogchangeSet
	// we know that oldKeys must exist here!
	ksRewind (oldKeys);
	ksRewind (returned);

	KeySet * addedKeys = ksDup (returned);
	KeySet * changedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);

	Key * k = 0;
	while ((k = ksNext (oldKeys)) != 0)
	{
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

	if (!strncmp (keyString (ksLookupByName (elektraPluginGetConfig (handle), "/announce", 0)), "once", 4))
	{
		if (!strncmp (keyName (parentKey), "user", 4))
			elektraDbusSendMessage (pluginData, DBUS_BUS_SESSION, keyName (parentKey), "Commit");
		if (!strncmp (keyName (parentKey), "system", 6))
			elektraDbusSendMessage (pluginData, DBUS_BUS_SYSTEM, keyName (parentKey), "Commit");
	}
	else
	{
		int announceAll = !strncmp (keyName (parentKey), "/", 1);
		int announceSession = !strncmp (keyName (parentKey), "user", 4);
		int announceSystem = !strncmp (keyName (parentKey), "system", 6);

		if (announceSession || announceAll)
		{
			announceKeys (addedKeys, "KeyAdded", DBUS_BUS_SESSION, pluginData);
			announceKeys (changedKeys, "KeyChanged", DBUS_BUS_SESSION, pluginData);
			announceKeys (removedKeys, "KeyDeleted", DBUS_BUS_SESSION, pluginData);
		}
		if (announceSystem || announceAll)
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

	// for next invocation of elektraLogchangeSet, remember our current keyset
	pluginData->keys = ksDup (returned);

	return 1; /* success */
}

int elektraDbusClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	ElektraDbusPluginData * pluginData = elektraPluginGetData (handle);

	KeySet * ks = pluginData->keys;
	if (ks) ksDel (ks);

	if (pluginData->systemBus)
	{
		if (pluginData->systemBusAdapter) elektraIoDbusAdapterCleanup (pluginData->systemBusAdapter);
		dbus_connection_unref (pluginData->systemBus);
	}
	if (pluginData->sessionBus)
	{
		if (pluginData->sessionBusAdapter) elektraIoDbusAdapterCleanup (pluginData->sessionBusAdapter);
		dbus_connection_unref (pluginData->sessionBus);
	}

	elektraFree (pluginData);

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT (dbus)
{
	// clang-format off
	return elektraPluginExport("dbus",
		ELEKTRA_PLUGIN_OPEN,	&elektraDbusOpen,
		ELEKTRA_PLUGIN_GET,	&elektraDbusGet,
		ELEKTRA_PLUGIN_SET,	&elektraDbusSet,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDbusClose,
		ELEKTRA_PLUGIN_END);
}
