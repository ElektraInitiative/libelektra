/**
 * @file
 *
 * @brief Source for dbus plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "dbus.h"

int elektraDbusGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/dbus"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/dbus", KEY_VALUE, "dbus plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/dbus/exports", KEY_END),
			       keyNew ("system/elektra/modules/dbus/exports/get", KEY_FUNC, elektraDbusGet, KEY_END),
			       keyNew ("system/elektra/modules/dbus/exports/set", KEY_FUNC, elektraDbusSet, KEY_END),
			       keyNew ("system/elektra/modules/dbus/exports/close", KEY_FUNC, elektraDbusClose, KEY_END),
#include ELEKTRA_README (dbus)
			       keyNew ("system/elektra/modules/dbus/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	// remember all keys
	KeySet * ks = (KeySet *)elektraPluginGetData (handle);
	if (ks)
		ksDel (ks);
	elektraPluginSetData (handle, ksDup (returned));

	return 1; /* success */
}

static void announceKeys (KeySet * ks, const char * signalName, DBusBusType busType)
{
	ksRewind (ks);
	Key * k = 0;
	while ((k = ksNext (ks)) != 0)
	{
		elektraDbusSendMessage (busType, keyName (k), signalName);
	}
}

int elektraDbusSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	KeySet * oldKeys = (KeySet *)elektraPluginGetData (handle);
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

	if (!strncmp (keyName (parentKey), "user", 4))
	{
		announceKeys (addedKeys, "KeyAdded", DBUS_BUS_SESSION);
		announceKeys (changedKeys, "KeyChanged", DBUS_BUS_SESSION);
		announceKeys (removedKeys, "KeyDeleted", DBUS_BUS_SESSION);
	}
	else if (!strncmp (keyName (parentKey), "system", 6))
	{
		announceKeys (addedKeys, "KeyAdded", DBUS_BUS_SYSTEM);
		announceKeys (changedKeys, "KeyChanged", DBUS_BUS_SYSTEM);
		announceKeys (removedKeys, "KeyDeleted", DBUS_BUS_SYSTEM);
	}

	ksDel (oldKeys);
	ksDel (addedKeys);
	ksDel (changedKeys);
	ksDel (removedKeys);

	// for next invocation of elektraLogchangeSet, remember our current keyset
	elektraPluginSetData (handle, ksDup (returned));

	return 1; /* success */
}

int elektraDbusClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	KeySet * ks = (KeySet *)elektraPluginGetData (handle);
	if (ks)
		ksDel (ks);
	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT (dbus)
{
	// clang-format off
	return elektraPluginExport("dbus",
		ELEKTRA_PLUGIN_GET,	&elektraDbusGet,
		ELEKTRA_PLUGIN_SET,	&elektraDbusSet,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDbusClose,
		ELEKTRA_PLUGIN_END);
}

