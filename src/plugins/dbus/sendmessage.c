/**
 * @file
 *
 * @brief Source for dbus plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "dbus.h"

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <kdblogger.h>

int elektraDbusSendMessage (DBusBusType type, const char * keyName, const char * signalName)
{
	DBusConnection * connection;
	DBusError error;
	DBusMessage * message;
	const char * dest = NULL; // to all receivers
	const char * interface = "org.libelektra";
	const char * path = "/org/libelektra/configuration";

	dbus_error_init (&error);
	connection = dbus_bus_get (type, &error);
	if (connection == NULL)
	{
		ELEKTRA_LOG_WARNING ("Failed to open connection to %s message bus: %s", (type == DBUS_BUS_SYSTEM) ? "system" : "session",
				     error.message);
		dbus_error_free (&error);
		return -1;
	}

	dbus_connection_set_exit_on_disconnect (connection, FALSE);

	message = dbus_message_new_signal (path, interface, signalName);

	if (message == NULL)
	{
		ELEKTRA_LOG_WARNING ("Couldn't allocate D-Bus message");
		dbus_connection_unref (connection);
		dbus_error_free (&error);
		return -1;
	}

	if (dest && !dbus_message_set_destination (message, dest))
	{
		ELEKTRA_LOG_WARNING ("Not enough memory");
		dbus_message_unref (message);
		dbus_connection_unref (connection);
		dbus_error_free (&error);
		return -1;
	}

	if (!dbus_message_append_args (message, DBUS_TYPE_STRING, &keyName, DBUS_TYPE_INVALID))
	{
		ELEKTRA_LOG_WARNING ("Couldn't add message argument");
		dbus_message_unref (message);
		dbus_connection_unref (connection);
		dbus_error_free (&error);
		return -1;
	}

	dbus_connection_send (connection, message, NULL);
	dbus_connection_flush (connection);

	dbus_message_unref (message);
	dbus_connection_unref (connection);
	dbus_error_free (&error);

	return 1;
}
