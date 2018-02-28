/**
 * @file
 *
 * @brief Source for dbus plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "dbus.h"

#include "../../bindings/io/adapter/dbus/dbus.h"

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <kdblogger.h>

/**
 * @internal
 * Get and setup D-Bus connection.
 *
 * If I/O binding is not NULL, handlePointer is updated to point to D-Bus I/O
 * adapter handle.
 *
 * @param  type          D-Bus bus type
 * @param  ioBinding     I/O binding (optional)
 * @param  handlePointer Pointer to D-Bus I/O adapter handle
 * @return D-Bus connection or NULL on error
 */
static DBusConnection * dbusGetConnection (DBusBusType type, ElektraIoInterface * ioBinding, ElektraIoDbusAdapterHandle ** handlePointer)
{
	DBusError error;
	dbus_error_init (&error);

	DBusConnection * connection = dbus_bus_get (type, &error);
	if (connection == NULL)
	{
		ELEKTRA_LOG_WARNING ("Failed to open connection to %s message bus: %s", (type == DBUS_BUS_SYSTEM) ? "system" : "session",
				     error.message);
		dbus_error_free (&error);
		return NULL;
	}
	dbus_error_free (&error);

	dbus_connection_set_exit_on_disconnect (connection, FALSE);

	if (ioBinding)
	{
		ElektraIoDbusAdapterHandle * handle = elektraIoDbusAdapterAttach (connection, ioBinding);
		if (!handle)
		{
			ELEKTRA_LOG_WARNING ("Failed to attach to the I/O binding");
			return NULL;
		}
		*handlePointer = handle;
	}

	return connection;
}

/**
 * @internal
 * Send Elektra's signal message over D-Bus.
 *
 * @param  pluginData Plugin data, stores D-Bus connection, I/O binding and more
 * @param  type       D-Bus bus type
 * @param  keyName    Key name to include in message
 * @param  signalName Signal name
 * @retval 1 on success
 * @retval -1 on error
 */
int elektraDbusSendMessage (ElektraDbusPluginData * pluginData, DBusBusType type, const char * keyName, const char * signalName)
{
	DBusConnection * connection;
	DBusMessage * message;
	const char * dest = NULL; // to all receivers
	const char * interface = "org.libelektra";
	const char * path = "/org/libelektra/configuration";

	switch (type)
	{
	case DBUS_BUS_SYSTEM:
		if (!pluginData->systemBus)
		{
			pluginData->systemBus = dbusGetConnection (type, pluginData->ioBinding, &pluginData->systemBusAdapter);
		}
		connection = pluginData->systemBus;
		break;
	case DBUS_BUS_SESSION:
		if (!pluginData->sessionBus)
		{
			pluginData->sessionBus = dbusGetConnection (type, pluginData->ioBinding, &pluginData->sessionBusAdapter);
		}
		connection = pluginData->sessionBus;
		break;
	default:
		connection = NULL;
	}
	if (connection == NULL)
	{
		return -1;
	}

	message = dbus_message_new_signal (path, interface, signalName);

	if (message == NULL)
	{
		ELEKTRA_LOG_WARNING ("Couldn't allocate D-Bus message");
		return -1;
	}

	if (dest && !dbus_message_set_destination (message, dest))
	{
		ELEKTRA_LOG_WARNING ("Not enough memory");
		dbus_message_unref (message);
		return -1;
	}

	if (!dbus_message_append_args (message, DBUS_TYPE_STRING, &keyName, DBUS_TYPE_INVALID))
	{
		ELEKTRA_LOG_WARNING ("Couldn't add message argument");
		dbus_message_unref (message);
		return -1;
	}

	dbus_connection_send (connection, message, NULL);

	dbus_message_unref (message);

	return 1;
}
