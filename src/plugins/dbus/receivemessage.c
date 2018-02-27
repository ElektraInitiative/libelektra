/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "dbus.h"

int elektraDbusSetupReceiveMessage (DBusConnection * connection, DBusHandleMessageFunction filter_func, void * data)
{
	DBusError error;
	dbus_error_init (&error);

	dbus_bus_add_match (connection, "type='signal',interface='org.libelektra',path='/org/libelektra/configuration'", &error);
	if (dbus_error_is_set (&error)) goto error;

	if (!dbus_connection_add_filter (connection, filter_func, data, NULL))
	{
		goto error;
	}

	dbus_error_free (&error);
	return 1;

error:
	printf ("Error occurred\n");
	dbus_error_free (&error);
	return -1;
}

int elektraDbusTeardownReceiveMessage (DBusConnection * connection, DBusHandleMessageFunction filter_func, void * data)
{
	DBusError error;
	dbus_error_init (&error);

	dbus_bus_remove_match (connection, "type='signal',interface='org.libelektra',path='/org/libelektra/configuration'", &error);
	if (dbus_error_is_set (&error)) goto error;

	dbus_connection_remove_filter (connection, filter_func, data);

	dbus_error_free (&error);
	return 1;

error:
	printf ("Error occurred\n");
	dbus_error_free (&error);
	return -1;
}

int elektraDbusReceiveMessage (DBusBusType type, DBusHandleMessageFunction filter_func)
{
	DBusConnection * connection;
	DBusError error;

	dbus_error_init (&error);
	connection = dbus_bus_get (type, &error);
	if (connection == NULL)
	{
		fprintf (stderr, "Failed to open connection to %s message bus: %s\n", (type == DBUS_BUS_SYSTEM) ? "system" : "session",
			 error.message);
		goto error;
	}

	int result = elektraDbusSetupReceiveMessage (connection, filter_func, NULL);
	if (!result) goto error;

	while (dbus_connection_read_write_dispatch (connection, -1))
		;
	return 0;
error:
	printf ("Error occurred\n");
	dbus_error_free (&error);
	return -1;
}
