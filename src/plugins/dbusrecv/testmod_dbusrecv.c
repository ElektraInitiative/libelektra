/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "dbus.h"

#include <stdio.h>  // printf() & co
#include <unistd.h> // sleep()
//#include <time.h> // time()

#include <kdbio_uv.h>
#include <kdbioplugin.h>

#include <uv.h>

#include <tests.h>
#include <tests_plugin.h>

#define TEST_TIMEOUT 500

Key * test_callbackKey;
uv_loop_t * test_callbackLoop;

static DBusConnection * getDbusConnection (DBusBusType type)
{
	DBusError error;
	dbus_error_init (&error);

	DBusConnection * connection = dbus_bus_get (type, &error);
	if (connection == NULL)
	{
		printf ("Failed to open connection to %s message bus: %s\n", (type == DBUS_BUS_SYSTEM) ? "system" : "session",
			error.message);
		dbus_error_free (&error);
		return NULL;
	}
	dbus_error_free (&error);

	dbus_connection_set_exit_on_disconnect (connection, FALSE);

	return connection;
}

static void dbusSendMessage (const char * signalName, const char * keyName)
{
	DBusMessage * message;
	const char * interface = "org.libelektra";
	const char * path = "/org/libelektra/configuration";

	DBusConnection * connection = getDbusConnection (DBUS_BUS_SYSTEM);
	exit_if_fail (connection != NULL, "could not get bus connection");

	message = dbus_message_new_signal (path, interface, signalName);
	exit_if_fail (message, "could not allocate dbus message");

	exit_if_fail (dbus_message_append_args (message, DBUS_TYPE_STRING, &keyName, DBUS_TYPE_INVALID), "could not add message arguments");

	dbus_connection_send (connection, message, NULL);

	dbus_message_unref (message);
	dbus_connection_unref (connection);

	return;
}

static void test_timerCallback (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	succeed_if (0, "timeout exceeded; test failed");
	uv_stop (test_callbackLoop);
}

static void test_notificationCallback (Key * key, ElektraNotificationCallbackContext * context ELEKTRA_UNUSED)
{
	test_callbackKey = key;
	uv_stop (test_callbackLoop);
}

static void test_prerequisites (void)
{
	printf ("testing prerequisites\n");

	DBusConnection * systemBus = getDbusConnection (DBUS_BUS_SYSTEM);
	succeed_if (systemBus != NULL, "could not open system bus");
	DBusConnection * sessionBus = getDbusConnection (DBUS_BUS_SESSION);
	succeed_if (sessionBus != NULL, "could not open session bus");

	exit_if_fail (systemBus != NULL, "could not get system message bus connection");

	if (systemBus) dbus_connection_unref (systemBus);
	if (sessionBus) dbus_connection_unref (sessionBus);
}

static void test_keyAdded (uv_loop_t * loop, ElektraIoInterface * binding)
{
	printf ("test adding keys\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("dbusrecv");

	// io binding is required for dispatching
	size_t func = elektraPluginGetFunction (plugin, "setIoBinding");
	exit_if_fail (func, "could not get function setIoBinding");
	ElektraIoPluginSetBinding setIoBinding = (ElektraIoPluginSetBinding)func;
	setIoBinding (plugin, binding);

	// open notification
	func = elektraPluginGetFunction (plugin, "openNotification");
	exit_if_fail (func, "could not get function openNotification");
	ElektraNotificationOpenNotification openNotification = (ElektraNotificationOpenNotification)func;
	openNotification (plugin, test_notificationCallback, NULL);

	char * expectedKeyName = "system/tests/testmod_dbusrecv/added";
	dbusSendMessage ("KeyAdded", expectedKeyName);

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (TEST_TIMEOUT, 1, test_timerCallback, NULL);
	elektraIoBindingAddTimer (binding, timerOp);

	test_callbackKey = NULL;
	test_callbackLoop = loop;
	uv_run (loop, UV_RUN_DEFAULT);

	succeed_if_same_string (expectedKeyName, keyName (test_callbackKey));

	// close notification
	func = elektraPluginGetFunction (plugin, "closeNotification");
	exit_if_fail (func, "could not get function closeNotification");
	ElektraNotificationCloseNotification closeNotification = (ElektraNotificationCloseNotification)func;
	closeNotification (plugin);

	elektraIoBindingRemoveTimer (timerOp);
	elektraFree (timerOp);
	keyDel (test_callbackKey);
	PLUGIN_CLOSE ();
}

static void test_keyChanged (uv_loop_t * loop, ElektraIoInterface * binding)
{
	printf ("test adding keys\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("dbusrecv");

	// io binding is required for dispatching
	size_t func = elektraPluginGetFunction (plugin, "setIoBinding");
	exit_if_fail (func, "could not get function setIoBinding");
	ElektraIoPluginSetBinding setIoBinding = (ElektraIoPluginSetBinding)func;
	setIoBinding (plugin, binding);

	// open notification
	func = elektraPluginGetFunction (plugin, "openNotification");
	exit_if_fail (func, "could not get function openNotification");
	ElektraNotificationOpenNotification openNotification = (ElektraNotificationOpenNotification)func;
	openNotification (plugin, test_notificationCallback, NULL);

	char * expectedKeyName = "system/tests/testmod_dbusrecv/changed";
	dbusSendMessage ("KeyChanged", expectedKeyName);

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (TEST_TIMEOUT, 1, test_timerCallback, NULL);
	elektraIoBindingAddTimer (binding, timerOp);

	test_callbackKey = NULL;
	test_callbackLoop = loop;
	uv_run (loop, UV_RUN_DEFAULT);

	succeed_if_same_string (expectedKeyName, keyName (test_callbackKey));

	// close notification
	func = elektraPluginGetFunction (plugin, "closeNotification");
	exit_if_fail (func, "could not get function closeNotification");
	ElektraNotificationCloseNotification closeNotification = (ElektraNotificationCloseNotification)func;
	closeNotification (plugin);

	elektraIoBindingRemoveTimer (timerOp);
	elektraFree (timerOp);
	keyDel (test_callbackKey);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("DBUSRECV TESTS\n");
	printf ("==============\n\n");

	init (argc, argv);

	// Test if dbus is available
	test_prerequisites ();

	uv_loop_t * loop = uv_default_loop ();
	ElektraIoInterface * binding = elektraIoUvNew (loop);

	// Test added, changed & deleted
	test_keyAdded (loop, binding);
	test_keyChanged (loop, binding);

	printf ("\ntestmod_dbusrecv RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	elektraIoBindingCleanup (binding);
	uv_run (loop, UV_RUN_ONCE);
	uv_loop_close (loop);

	dbus_shutdown ();

	return nbError;
}
