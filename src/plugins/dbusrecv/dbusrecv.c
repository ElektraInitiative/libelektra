/**
 * @file
 *
 * @brief Source for dbusrecv plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include <internal/config.h>
#endif

#include "./dbusrecv.h"

#include <internal/utility/alloc.h>
#include <internal/utility/logger.h>
#include <stdio.h>

static int setupNotificationCallback (Plugin * handle)
{
	ELEKTRA_NOT_NULL (handle);
	ElektraDbusRecvPluginData * pluginData = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (pluginData);

	KeySet * global = elektraPluginGetGlobalKeySet (handle);

	ElektraNotificationCallback callback;
	Key * callbackKey = ksLookupByName (global, "system:/elektra/notification/callback", 0);
	const void * callbackPtr = keyValue (callbackKey);

	if (callbackPtr == NULL)
	{
		return -1;
	}

	callback = *(ElektraNotificationCallback *) keyValue (callbackKey);

	ElektraNotificationCallbackContext * context;
	Key * contextKey = ksLookupByName (global, "system:/elektra/notification/context", 0);
	const void * contextPtr = keyValue (contextKey);
	context = contextPtr == NULL ? NULL : *(ElektraNotificationCallbackContext **) contextPtr;


	pluginData->notificationCallback = callback;
	pluginData->notificationContext = context;

	return 0;
}

/**
 * @internal
 * Process D-Bus messages and check for Elektra's signal messages.
 *
 * Only KeyChanged and KeyAdded are processed.
 *
 * @param  connection	D-Bus connection
 * @param  message    message
 * @param  data       plugin data
 * @return            handler result
 */
DBusHandlerResult elektraDbusRecvMessageHandler (DBusConnection * connection ELEKTRA_UNUSED, DBusMessage * message, void * data)
{
	char * interface = "org.libelektra";

	int processMessage = dbus_message_is_signal (message, interface, "Commit") ||
			     dbus_message_is_signal (message, interface, "KeyAdded") ||
			     dbus_message_is_signal (message, interface, "KeyChanged");
	if (processMessage)
	{
		Plugin * handle = (Plugin *) data;
		ELEKTRA_NOT_NULL (handle);
		ElektraDbusRecvPluginData * pluginData = elektraPluginGetData (handle);
		ELEKTRA_NOT_NULL (pluginData);

		if (pluginData->notificationCallback == NULL)
		{
			if (setupNotificationCallback (handle) != 0)
			{
				ELEKTRA_LOG_WARNING ("notificationCallback not set up, ignoring dbus message");
				return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
			}
		}

		char * keyName;
		DBusError error;
		dbus_error_init (&error);

		// read key name from message
		dbus_message_get_args (message, &error, DBUS_TYPE_STRING, &keyName, DBUS_TYPE_INVALID);
		if (dbus_error_is_set (&error))
		{
			ELEKTRA_LOG_WARNING ("Failed to read message: %s", error.message);
		}
		else
		{
			Key * changed = keyNew (keyName, KEY_END);
			pluginData->notificationCallback (changed, pluginData->notificationContext);
		}

		dbus_error_free (&error);
		return DBUS_HANDLER_RESULT_HANDLED;
	}
	return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

int elektraDbusRecvOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	ElektraDbusRecvPluginData * data = (ElektraDbusRecvPluginData *) elektraPluginGetData (handle);
	if (data == NULL)
	{
		data = elektraMalloc (sizeof (*data));
		data->ioBinding = NULL;
		data->dbusInitialized = 0;
		data->systemBus = NULL;
		data->sessionBus = NULL;
		data->notificationCallback = NULL;
		data->notificationContext = NULL;
		elektraPluginSetData (handle, data);
	}

	if (data->ioBinding == NULL)
	{
		KeySet * global = elektraPluginGetGlobalKeySet (handle);

		Key * ioBindingKey = ksLookupByName (global, "system:/elektra/io/binding", 0);
		const void * bindingPtr = keyValue (ioBindingKey);
		ElektraIoInterface * binding = bindingPtr == NULL ? NULL : *(ElektraIoInterface **) keyValue (ioBindingKey);

		data->ioBinding = binding;
	}

	// init dbus connections
	if (data->ioBinding)
	{
		int result;

		result = elektraDbusRecvSetupReceive (handle, DBUS_BUS_SYSTEM, elektraDbusRecvMessageHandler);
		if (!result)
		{
			ELEKTRA_LOG_WARNING ("setup for system bus failed!");
		}
		result = elektraDbusRecvSetupReceive (handle, DBUS_BUS_SESSION, elektraDbusRecvMessageHandler);
		if (!result)
		{
			ELEKTRA_LOG_WARNING ("setup for session bus failed!");
		}

		data->dbusInitialized = 1;
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("no I/O binding present. plugin in noop mode");
	}

	return 1; /* success */
}

int elektraDbusRecvGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/dbusrecv"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/dbusrecv", KEY_VALUE, "dbusrecv plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/dbusrecv/exports", KEY_END),
			       keyNew ("system:/elektra/modules/dbusrecv/exports/open", KEY_FUNC, elektraDbusRecvOpen, KEY_END),
			       keyNew ("system:/elektra/modules/dbusrecv/exports/get", KEY_FUNC, elektraDbusRecvGet, KEY_END),
			       keyNew ("system:/elektra/modules/dbusrecv/exports/close", KEY_FUNC, elektraDbusRecvClose, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/dbusrecv/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	return 1; /* success */
}

int elektraDbusRecvClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	ElektraDbusRecvPluginData * pluginData = elektraPluginGetData (handle);
	if (pluginData == NULL)
	{
		return 1;
	}

	if (pluginData->dbusInitialized)
	{
		int result;
		result = elektraDbusRecvTeardownReceive (handle, DBUS_BUS_SYSTEM, elektraDbusRecvMessageHandler);
		if (!result)
		{
			ELEKTRA_LOG_WARNING ("teardown for system bus failed!");
		}
		result = elektraDbusRecvTeardownReceive (handle, DBUS_BUS_SESSION, elektraDbusRecvMessageHandler);
		if (!result)
		{
			ELEKTRA_LOG_WARNING ("teardown for session bus failed!");
		}

		pluginData->dbusInitialized = 0;
	}

	if (pluginData->systemBus)
	{
		elektraIoAdapterDbusCleanup (pluginData->systemBusAdapter);
		dbus_connection_close (pluginData->systemBus);
		dbus_connection_unref (pluginData->systemBus);
		pluginData->systemBus = NULL;
	}
	if (pluginData->sessionBus)
	{
		elektraIoAdapterDbusCleanup (pluginData->sessionBusAdapter);
		dbus_connection_close (pluginData->sessionBus);
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
	return elektraPluginExport("dbusrecv",
		ELEKTRA_PLUGIN_OPEN,	&elektraDbusRecvOpen,
		ELEKTRA_PLUGIN_GET,	&elektraDbusRecvGet,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDbusRecvClose,
		ELEKTRA_PLUGIN_END);
}
