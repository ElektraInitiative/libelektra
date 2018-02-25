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

#include "../../bindings/io/adapter/dbus/dbus.h"
#include "dbusrecv.h"

#include <kdbhelper.h>
#include <kdblogger.h>

#include <stdio.h>

void elektraDbusRecvSetIoBinding (Plugin * handle, ElektraIoInterface * binding)
{
	ElektraDbusRecvPluginData * data = elektraPluginGetData (handle);
	data->ioBinding = binding;
}

void elektraDbusRecvOpenNotification (Plugin * handle, ElektraNotificationCallback callback, void * data)
{
	ElektraDbusRecvPluginData * pluginData = elektraPluginGetData (handle);

	pluginData->notificationCallback = callback;
	pluginData->notificationPayload = data;

	// init dbus connections
	if (pluginData->ioBinding && !pluginData->dbusInitialized)
	{
		int result;

		result = elektraDbusRecvSetupReceive (pluginData, DBUS_BUS_SYSTEM, elektraDbusRecvMessageHandler);
		if (!result)
		{
			ELEKTRA_LOG_WARNING ("setup for system bus failed!");
		}
		result = elektraDbusRecvSetupReceive (pluginData, DBUS_BUS_SESSION, elektraDbusRecvMessageHandler);
		if (!result)
		{
			ELEKTRA_LOG_WARNING ("setup for session bus failed!");
		}

		pluginData->dbusInitialized = 1;
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("no I/O binding present. plugin in noop mode");
	}
}

void elektraDbusRecvCloseNotification (Plugin * handle)
{
	ElektraDbusRecvPluginData * pluginData = elektraPluginGetData (handle);
	pluginData->notificationCallback = NULL;
	pluginData->notificationPayload = NULL;

	if (pluginData->dbusInitialized)
	{
		int result;
		result = elektraDbusRecvTeardownReceive (pluginData, DBUS_BUS_SYSTEM, elektraDbusRecvMessageHandler);
		if (!result)
		{
			ELEKTRA_LOG_WARNING ("teardown for system bus failed!");
		}
		result = elektraDbusRecvTeardownReceive (pluginData, DBUS_BUS_SESSION, elektraDbusRecvMessageHandler);
		if (!result)
		{
			ELEKTRA_LOG_WARNING ("teardown for session bus failed!");
		}
	}
}

DBusHandlerResult elektraDbusRecvMessageHandler (DBusConnection * connection ELEKTRA_UNUSED, DBusMessage * message, void * data)
{
	DBusMessageIter args;
	char * keyName;

	char * interface = "org.libelektra";

	if (dbus_message_is_signal (message, interface, "KeyChanged"))
	{
		// read the parameters
		if (!dbus_message_iter_init (message, &args))
		{
			ELEKTRA_LOG_WARNING ("malformed message received");
		}
		else
		{
			if (DBUS_TYPE_STRING == dbus_message_iter_get_arg_type (&args))
			{
				dbus_message_iter_get_basic (&args, &keyName);
			}
			dbus_message_iter_next (&args);
		}

		Key * changed = keyNew (keyName, KEY_END);

		ElektraDbusRecvPluginData * pluginData = (ElektraDbusRecvPluginData *)data;
		pluginData->notificationCallback (changed, pluginData->notificationPayload);

		return DBUS_HANDLER_RESULT_HANDLED;
	}
	return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

int elektraDbusRecvOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	ElektraDbusRecvPluginData * data = (ElektraDbusRecvPluginData *)elektraPluginGetData (handle);
	if (!data)
	{
		data = elektraMalloc (sizeof (*data));
		data->ioBinding = NULL;
		data->notificationCallback = NULL;
		data->dbusInitialized = 0;
		data->systemBus = NULL;
		data->sessionBus = NULL;
	}
	elektraPluginSetData (handle, data);

	return 1; /* success */
}

int elektraDbusRecvGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/dbusrecv"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/dbusrecv", KEY_VALUE, "dbusrecv plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/dbusrecv/exports", KEY_END),
			keyNew ("system/elektra/modules/dbusrecv/exports/open", KEY_FUNC, elektraDbusRecvOpen, KEY_END),
			keyNew ("system/elektra/modules/dbusrecv/exports/get", KEY_FUNC, elektraDbusRecvGet, KEY_END),
			keyNew ("system/elektra/modules/dbusrecv/exports/close", KEY_FUNC, elektraDbusRecvClose, KEY_END),
			keyNew ("system/elektra/modules/dbusrecv/exports/setIoBinding", KEY_FUNC, elektraDbusRecvSetIoBinding, KEY_END),
			keyNew ("system/elektra/modules/dbusrecv/exports/openNotification", KEY_FUNC, elektraDbusRecvOpenNotification,
				KEY_END),
			keyNew ("system/elektra/modules/dbusrecv/exports/closeNotification", KEY_FUNC, elektraDbusRecvCloseNotification,
				KEY_END),
#include ELEKTRA_README (dbusrecv)
			keyNew ("system/elektra/modules/dbusrecv/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	return 1; /* success */
}

int elektraDbusRecvClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	ElektraDbusRecvPluginData * pluginData = elektraPluginGetData (handle);

	if (pluginData->systemBus)
	{
		elektraIoDbusAdapterCleanup (pluginData->systemBusAdapter);
		dbus_connection_close (pluginData->systemBus);
	}
	if (pluginData->sessionBus)
	{
		elektraIoDbusAdapterCleanup (pluginData->sessionBusAdapter);
		dbus_connection_close (pluginData->sessionBus);
	}

	elektraFree (pluginData);

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT (dbus)
{
	// clang-format off
	return elektraPluginExport("dbusrecv",
		ELEKTRA_PLUGIN_OPEN,	&elektraDbusRecvOpen,
		ELEKTRA_PLUGIN_GET,	&elektraDbusRecvGet,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDbusRecvClose,
		ELEKTRA_PLUGIN_END);
}
