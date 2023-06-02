/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./dbus.h"

#ifdef HAVE_KDBCONFIG_H
#include <internal/config.h>
#endif

#include <internal/utility/alloc.h>
#include <stdio.h>

void print_message (DBusMessage * message, dbus_bool_t literal);

DBusHandlerResult callback (DBusConnection * connection ELEKTRA_UNUSED, DBusMessage * message, void * user_data ELEKTRA_UNUSED)
{
	if (dbus_message_is_signal (message, DBUS_INTERFACE_DBUS, "NameAcquired")) return DBUS_HANDLER_RESULT_HANDLED;

	if (dbus_message_is_signal (message, DBUS_INTERFACE_LOCAL, "Disconnected")) return DBUS_HANDLER_RESULT_HANDLED;

	printf ("Notify received\n");
	return DBUS_HANDLER_RESULT_HANDLED;
}

int main (int argc, char ** argv)
{
	if (argc == 2)
	{
		ElektraDbusPluginData * data = elektraCalloc (sizeof *data);
		if (!strcmp (argv[1], "send_session")) elektraDbusSendMessage (data, DBUS_BUS_SESSION, "test1", "KeyChanged");
		if (!strcmp (argv[1], "send_system")) elektraDbusSendMessage (data, DBUS_BUS_SYSTEM, "test2", "KeyChanged");
		if (!strcmp (argv[1], "receive_session")) elektraDbusReceiveMessage (DBUS_BUS_SESSION, callback);
		if (!strcmp (argv[1], "receive_system")) elektraDbusReceiveMessage (DBUS_BUS_SYSTEM, callback);
	}
}
