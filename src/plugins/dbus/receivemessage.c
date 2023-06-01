/**
 * @file
 *
 * @brief Test helpers for dbus plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./dbus.h"

#include <stdio.h>

#define RECEIVE_MATCH_RULE "type='signal',interface='org.libelektra',path='/org/libelektra/configuration'"

/**
 * @internal
 * Setup D-Bus connection for receiving Elektra's signal messages.
 *
 * @param  connection  D-Bus connection
 * @param  filter_func message handler
 * @param  data        data passed to message handler
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraDbusSetupReceiveMessage (DBusConnection * connection, DBusHandleMessageFunction filter_func, void * data)
{
	ELEKTRA_NOT_NULL (connection);
	ELEKTRA_NOT_NULL (filter_func);

	DBusError error;
	dbus_error_init (&error);

	dbus_bus_add_match (connection, RECEIVE_MATCH_RULE, &error);
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
	return 0;
}

/**
 * @internal
 * Revert changes made to D-Bus connection by elektraDbusSetupReceiveMessage().
 *
 * @param  connection  D-Bus connection
 * @param  filter_func message handler
 * @param  data        data passed to message handler
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraDbusTeardownReceiveMessage (DBusConnection * connection, DBusHandleMessageFunction filter_func, void * data)
{
	ELEKTRA_NOT_NULL (connection);
	ELEKTRA_NOT_NULL (filter_func);

	DBusError error;
	dbus_error_init (&error);

	dbus_bus_remove_match (connection, RECEIVE_MATCH_RULE, &error);
	if (dbus_error_is_set (&error)) goto error;

	dbus_connection_remove_filter (connection, filter_func, data);

	dbus_error_free (&error);
	return 1;

error:
	printf ("Error occurred\n");
	dbus_error_free (&error);
	return 0;
}

/**
 * Setup receiving of Elektra's D-Bus signal messages and do blocking dispatch.
 *
 * Messages are passed to filter_func
 *
 * @param  type        D-Bus bus type
 * @param  filter_func message handler
 * @retval 0 on success
 * @retval -1 on error
 */
int elektraDbusReceiveMessage (DBusBusType type, DBusHandleMessageFunction filter_func)
{
	ELEKTRA_NOT_NULL (filter_func);

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
