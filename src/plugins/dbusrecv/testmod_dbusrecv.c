/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "dbusrecv.h"

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>

void print_message (DBusMessage * message, dbus_bool_t literal);

DBusHandlerResult callback (DBusConnection * connection ELEKTRA_UNUSED, DBusMessage * message, void * user_data ELEKTRA_UNUSED)
{
	if (dbus_message_is_signal (message, DBUS_INTERFACE_DBUS, "NameAcquired")) return DBUS_HANDLER_RESULT_HANDLED;

	if (dbus_message_is_signal (message, DBUS_INTERFACE_LOCAL, "Disconnected")) return DBUS_HANDLER_RESULT_HANDLED;

	printf ("Notify received\n");
	return DBUS_HANDLER_RESULT_HANDLED;
}

int main (int argc, char ** argv ELEKTRA_UNUSED)
{
	if (argc == 2)
	{
		// ElektraDbusRecvPluginData data;
		// if (!strcmp (argv[1], "receive_session")) elektraDbusRecvReceiveMessage (DBUS_BUS_SESSION, callback);
		// if (!strcmp (argv[1], "receive_system")) elektraDbusRecvReceiveMessage (DBUS_BUS_SYSTEM, callback);
	}
}
