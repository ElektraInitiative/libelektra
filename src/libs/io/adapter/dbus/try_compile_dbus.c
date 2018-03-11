/**
 * @file
 *
 * @brief Compilation test for D-Bus.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#include <dbus/dbus.h>

#include <stdlib.h>

int main (void)
{
	DBusError error;
	dbus_error_init (&error);
	DBusConnection * connection = dbus_bus_get (DBUS_BUS_SYSTEM, &error);
	dbus_error_free (&error);
	dbus_connection_unref (connection);
	return 0;
}
