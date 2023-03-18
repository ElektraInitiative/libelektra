/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "dbusrecv.h"

#include <stdio.h> // printf() & co

#include <elektra/io/uv.h>
#include <elektra/io/plugin.h>

#include <uv.h>

#include <tests.h>
#include <tests_plugin.h>

#define TEST_TIMEOUT 1000

Key * test_callbackKey;
uv_loop_t * test_callbackLoop;

/** D-Bus bus type used by tests  */
DBusBusType testBusType;

/**
 * @internal
 * Get and setup D-Bus connection
 *
 * @param  type D-Bus bus type
 * @return      D-Bus connection or NULL on error
 */
static DBusConnection * getDbusConnection (DBusBusType type)
{
	DBusError error;
	dbus_error_init (&error);

	DBusConnection * connection = dbus_bus_get (type, &error);
	if (connection == NULL)
	{
		printf ("connect: Failed to open connection to %s message bus: %s\n", (type == DBUS_BUS_SYSTEM) ? "system" : "session",
			error.message);
		dbus_error_free (&error);
		return NULL;
	}
	dbus_error_free (&error);

	dbus_connection_set_exit_on_disconnect (connection, FALSE);

	return connection;
}

/**
 * @internal
 * Send Elektra's D-Bus message.
 *
 * @param signalName signal name
 * @param keyName    key name
 */
static void dbusSendMessage (const char * signalName, const char * keyName)
{
	DBusMessage * message;
	const char * interface = "org.libelektra";
	const char * path = "/org/libelektra/configuration";

	DBusConnection * connection = getDbusConnection (testBusType);
	exit_if_fail (connection != NULL, "could not get bus connection");

	message = dbus_message_new_signal (path, interface, signalName);
	exit_if_fail (message, "could not allocate dbus message");

	exit_if_fail (dbus_message_append_args (message, DBUS_TYPE_STRING, &keyName, DBUS_TYPE_INVALID), "could not add message arguments");

	dbus_connection_send (connection, message, NULL);

	dbus_message_unref (message);
	dbus_connection_unref (connection);

	return;
}

/**
 * Timeout for tests.
 *
 * Creates a failure and stops the event loop
 *
 * @param timerOp timer operation
 */
static void test_timerCallback (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	succeed_if (0, "timeout exceeded; test failed");
	uv_stop (test_callbackLoop);
}

/**
 * @internal
 * Called by plugin when a notification was received.
 * The key is saved to be evaluated by the current test and the event loop is
 * stopped.
 *
 * @see ElektraNotificationCallback (kdbnotificationinternal.h)
 *
 * @param key     changed key
 * @param context notification callback context
 */
static void test_notificationCallback (Key * key, ElektraNotificationCallbackContext * context ELEKTRA_UNUSED)
{
	test_callbackKey = key;
	uv_stop (test_callbackLoop);
}

static int test_prerequisites (void)
{
	printf ("testing prerequisites\n");
	printf ("detecting available bus types - please ignore single error messages prefixed with \"connect:\"\n");

	DBusConnection * systemBus = getDbusConnection (DBUS_BUS_SYSTEM);
	DBusConnection * sessionBus = getDbusConnection (DBUS_BUS_SESSION);

	int success = 0;
	if (systemBus != NULL || sessionBus != NULL)
	{
		// Set bus type for tests
		// NOTE brew dbus on MacOs supports session by out of the box while session
		// bus is not available without further configuration on Linux
		if (systemBus)
		{
			testBusType = DBUS_BUS_SYSTEM;
		}
		else if (sessionBus)
		{
			testBusType = DBUS_BUS_SESSION;
		}
		success = 1;
	}

	if (systemBus) dbus_connection_unref (systemBus);
	if (sessionBus) dbus_connection_unref (sessionBus);

	return success;
}

static void test_commit (uv_loop_t * loop, ElektraIoInterface * binding)
{
	printf ("test commit\n");

	KeySet * conf = ksNew (1, keyNew ("user:/announce", KEY_VALUE, "once", KEY_END), KS_END);
	PLUGIN_OPEN ("dbusrecv");

	ksDel (plugin->global);
	plugin->global =
		ksNew (5, keyNew ("system:/elektra/io/binding", KEY_BINARY, KEY_SIZE, sizeof (binding), KEY_VALUE, &binding, KEY_END),
		       keyNew ("system:/elektra/notification/callback", KEY_FUNC, test_notificationCallback, KEY_END), KS_END);
	// call open again after correctly setting up global keyset
	plugin->kdbOpen (plugin, NULL);

	char * expectedKeyName = "system:/tests/testmod_dbusrecv/changed";
	dbusSendMessage ("Commit", expectedKeyName);

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (TEST_TIMEOUT, 1, test_timerCallback, NULL);
	elektraIoBindingAddTimer (binding, timerOp);

	test_callbackKey = NULL;
	test_callbackLoop = loop;
	uv_run (loop, UV_RUN_DEFAULT);

	succeed_if_same_string (expectedKeyName, keyName (test_callbackKey));

	elektraIoBindingRemoveTimer (timerOp);
	elektraFree (timerOp);
	keyDel (test_callbackKey);
	ksDel (plugin->global);
	PLUGIN_CLOSE ();
}

static void test_keyAdded (uv_loop_t * loop, ElektraIoInterface * binding)
{
	printf ("test adding keys\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("dbusrecv");

	ksDel (plugin->global);
	plugin->global =
		ksNew (5, keyNew ("system:/elektra/io/binding", KEY_BINARY, KEY_SIZE, sizeof (binding), KEY_VALUE, &binding, KEY_END),
		       keyNew ("system:/elektra/notification/callback", KEY_FUNC, test_notificationCallback, KEY_END), KS_END);
	// call open again after correctly setting up global keyset
	plugin->kdbOpen (plugin, NULL);

	char * expectedKeyName = "system:/tests/testmod_dbusrecv/added";
	dbusSendMessage ("KeyAdded", expectedKeyName);

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (TEST_TIMEOUT, 1, test_timerCallback, NULL);
	elektraIoBindingAddTimer (binding, timerOp);

	test_callbackKey = NULL;
	test_callbackLoop = loop;
	uv_run (loop, UV_RUN_DEFAULT);

	succeed_if_same_string (expectedKeyName, keyName (test_callbackKey));

	elektraIoBindingRemoveTimer (timerOp);
	elektraFree (timerOp);
	keyDel (test_callbackKey);
	ksDel (plugin->global);
	PLUGIN_CLOSE ();
}

static void test_keyChanged (uv_loop_t * loop, ElektraIoInterface * binding)
{
	printf ("test changing keys\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("dbusrecv");

	ksDel (plugin->global);
	plugin->global =
		ksNew (5, keyNew ("system:/elektra/io/binding", KEY_BINARY, KEY_SIZE, sizeof (binding), KEY_VALUE, &binding, KEY_END),
		       keyNew ("system:/elektra/notification/callback", KEY_FUNC, test_notificationCallback, KEY_END), KS_END);
	// call open again after correctly setting up global keyset
	plugin->kdbOpen (plugin, NULL);

	char * expectedKeyName = "system:/tests/testmod_dbusrecv/changed";
	dbusSendMessage ("KeyChanged", expectedKeyName);

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (TEST_TIMEOUT, 1, test_timerCallback, NULL);
	elektraIoBindingAddTimer (binding, timerOp);

	test_callbackKey = NULL;
	test_callbackLoop = loop;
	uv_run (loop, UV_RUN_DEFAULT);

	succeed_if_same_string (expectedKeyName, keyName (test_callbackKey));

	elektraIoBindingRemoveTimer (timerOp);
	elektraFree (timerOp);
	keyDel (test_callbackKey);
	ksDel (plugin->global);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("DBUSRECV TESTS\n");
	printf ("==============\n\n");

	init (argc, argv);

	// Test if dbus is available
	if (test_prerequisites ())
	{

		uv_loop_t * loop = uv_default_loop ();
		ElektraIoInterface * binding = elektraIoUvNew (loop);

		// Test change types
		test_commit (loop, binding);
		test_keyAdded (loop, binding);
		test_keyChanged (loop, binding);

		elektraIoBindingCleanup (binding);
		uv_run (loop, UV_RUN_ONCE);
#ifdef HAVE_LIBUV1
		uv_loop_close (loop);
#elif HAVE_LIBUV0
		uv_loop_delete (loop);
#endif
	}
	else
	{
		printf ("warning: no dbus daemon available; skipping tests that require dbus\n");
	}

	print_result ("testmod_dbusrecv");

	dbus_shutdown ();

	return nbError;
}
