/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "dbus.h"

#include <stdio.h> // printf() & co
#include <time.h>  // time()

#include <tests.h>
#include <tests_plugin.h>

typedef struct
{
	char * lookupSignalName;
	char * receivedKeyName;

	DBusConnection * connection;
	int stop;
} ReceiveContext;

DBusHandlerResult receiveMessageHandler (DBusConnection * connection ELEKTRA_UNUSED, DBusMessage * message, void * data)
{
	ReceiveContext * context = (ReceiveContext *)data;

	char * interface = "org.libelektra";
	if (dbus_message_is_signal (message, interface, context->lookupSignalName))
	{
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
			// Message received, stop dispatching
			context->receivedKeyName = keyName;
			context->stop = 1;
		}

		dbus_error_free (&error);
		return DBUS_HANDLER_RESULT_HANDLED;
	}
	return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static void runDispatch (ReceiveContext * context)
{
	time_t now;
	time_t start = time (NULL);
	context->stop = 0;
	while (!context->stop && dbus_connection_read_write_dispatch (context->connection, 100))
	{
		now = time (NULL);
		// Stop dispatching after one second
		if (now - start > 1)
		{
			succeed_if (0, "timeout exceeded; test failed");
			break;
		}
	}
}

static ReceiveContext * createReceiveContext (DBusConnection * connection, char * signalName)
{
	ReceiveContext * context = elektraMalloc (sizeof *context);
	exit_if_fail (context, "malloc failed");

	context->lookupSignalName = signalName;
	context->connection = connection;
	context->receivedKeyName = "";
	return context;
}

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

static void test_prerequisites (void)
{
	printf ("testing prerequisites\n");

	DBusConnection * systemBus = getDbusConnection (DBUS_BUS_SYSTEM);

	exit_if_fail (systemBus != NULL, "could not get system message bus connection");

	dbus_connection_unref (systemBus);
}

static void test_keyAdded (void)
{
	printf ("test adding keys\n");

	Key * toAdd = keyNew ("system/tests/foo/bar", KEY_VALUE, "test", KEY_END);
	KeySet * ks = ksNew (0, KS_END);
	Key * parentKey = keyNew ("system/tests/foo", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("dbus");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// add key to keyset
	ksAppendKey (ks, toAdd);

	DBusConnection * connection = getDbusConnection (DBUS_BUS_SYSTEM);
	ReceiveContext * context = createReceiveContext (connection, "KeyAdded");
	elektraDbusSetupReceiveMessage (connection, receiveMessageHandler, (void *)context);

	plugin->kdbSet (plugin, ks, parentKey);
	runDispatch (context);

	succeed_if_same_string (keyName (toAdd), context->receivedKeyName);

	elektraFree (context);
	elektraDbusTeardownReceiveMessage (connection, receiveMessageHandler, (void *)context);
	dbus_connection_unref (connection);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_keyChanged (void)
{
	printf ("test changing keys\n");

	// All keys created by keyNew have the KEY_FLAG_SYNC set and will be
	// detected as changed by the dbus plugin
	// This flag is only cleared after kdbSet or when keys come from a backend.
	Key * toChange = keyNew ("system/tests/foo/bar", KEY_VALUE, "test", KEY_END);
	KeySet * ks = ksNew (2, toChange, KS_END);
	Key * parentKey = keyNew ("system/tests/foo", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("dbus");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// change key in keyset
	keySetString (toChange, "new value");

	DBusConnection * connection = getDbusConnection (DBUS_BUS_SYSTEM);
	ReceiveContext * context = createReceiveContext (connection, "KeyChanged");
	elektraDbusSetupReceiveMessage (connection, receiveMessageHandler, (void *)context);

	plugin->kdbSet (plugin, ks, parentKey);
	runDispatch (context);

	succeed_if_same_string (keyName (toChange), context->receivedKeyName);

	elektraFree (context);
	elektraDbusTeardownReceiveMessage (connection, receiveMessageHandler, (void *)context);
	dbus_connection_unref (connection);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_keyDeleted (void)
{
	printf ("test deleting keys\n");

	Key * toDelete = keyNew ("system/tests/foo/bar", KEY_VALUE, "test", KEY_END);
	KeySet * ks = ksNew (1, keyDup (toDelete), KS_END);
	Key * parentKey = keyNew ("system/tests/foo", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("dbus");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// remove key from keyset
	Key * deleted = ksLookup (ks, toDelete, KDB_O_POP);
	succeed_if (deleted != NULL, "key was not found");

	DBusConnection * connection = getDbusConnection (DBUS_BUS_SYSTEM);
	ReceiveContext * context = createReceiveContext (connection, "KeyDeleted");
	elektraDbusSetupReceiveMessage (connection, receiveMessageHandler, (void *)context);

	plugin->kdbSet (plugin, ks, parentKey);
	runDispatch (context);

	succeed_if_same_string (keyName (toDelete), context->receivedKeyName);

	elektraFree (context);
	elektraDbusTeardownReceiveMessage (connection, receiveMessageHandler, (void *)context);
	dbus_connection_unref (connection);
	keyDel (toDelete);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_announceOnce (void)
{
	printf ("test announce once\n");

	Key * toAdd1 = keyNew ("system/tests/foo/bar/#0", KEY_VALUE, "test", KEY_END);
	Key * toAdd2 = keyNew ("system/tests/foo/bar/#1", KEY_VALUE, "test", KEY_END);
	Key * toChange = keyNew ("system/tests/foo/bar", KEY_VALUE, "test", KEY_END);
	KeySet * ks = ksNew (1, toChange, KS_END);
	Key * parentKey = keyNew ("system/tests/foo", KEY_END);

	KeySet * conf = ksNew (1, keyNew ("/announce", KEY_VALUE, "once", KEY_END), KS_END);
	PLUGIN_OPEN ("dbus");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// modify keyset
	ksAppendKey (ks, toAdd1);
	ksAppendKey (ks, toAdd2);
	keySetString (toChange, "new value");

	DBusConnection * connection = getDbusConnection (DBUS_BUS_SYSTEM);
	ReceiveContext * context = createReceiveContext (connection, "Commit");
	elektraDbusSetupReceiveMessage (connection, receiveMessageHandler, (void *)context);

	plugin->kdbSet (plugin, ks, parentKey);
	runDispatch (context);

	succeed_if_same_string (keyName (parentKey), context->receivedKeyName);

	elektraFree (context);
	elektraDbusTeardownReceiveMessage (connection, receiveMessageHandler, (void *)context);
	dbus_connection_unref (connection);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_cascadedChangeNotification (void)
{
	printf ("test change notification with cascaded parent key\n");

	Key * toAdd = keyNew ("system/tests/foo/bar", KEY_VALUE, "test", KEY_END);
	Key * completeParentKey = keyNew ("system/tests/foo", KEY_END);
	KeySet * ks = ksNew (1, completeParentKey, KS_END);
	Key * parentKey = keyNew ("/tests/foo", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("dbus");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// add key to keyset
	ksAppendKey (ks, toAdd);

	DBusConnection * connection = getDbusConnection (DBUS_BUS_SYSTEM);
	ReceiveContext * context = createReceiveContext (connection, "KeyAdded");
	elektraDbusSetupReceiveMessage (connection, receiveMessageHandler, (void *)context);

	plugin->kdbSet (plugin, ks, parentKey);
	runDispatch (context);

	succeed_if_same_string (keyName (toAdd), context->receivedKeyName);

	elektraFree (context);
	elektraDbusTeardownReceiveMessage (connection, receiveMessageHandler, (void *)context);
	dbus_connection_unref (connection);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_cascadedAnnounceOnce (void)
{
	printf ("test announce once with cascaded parent key\n");

	Key * toAdd = keyNew ("system/tests/foo/bar", KEY_VALUE, "test", KEY_END);
	Key * completeParentKey = keyNew ("system/tests/foo", KEY_END);
	KeySet * ks = ksNew (1, completeParentKey, KS_END);
	Key * parentKey = keyNew ("/tests/foo", KEY_END);

	KeySet * conf = ksNew (1, keyNew ("/announce", KEY_VALUE, "once", KEY_END), KS_END);
	PLUGIN_OPEN ("dbus");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// add key to keyset
	ksAppendKey (ks, toAdd);

	DBusConnection * connection = getDbusConnection (DBUS_BUS_SYSTEM);
	ReceiveContext * context = createReceiveContext (connection, "Commit");
	elektraDbusSetupReceiveMessage (connection, receiveMessageHandler, (void *)context);

	plugin->kdbSet (plugin, ks, parentKey);
	runDispatch (context);

	succeed_if_same_string (keyName (completeParentKey), context->receivedKeyName);

	elektraFree (context);
	elektraDbusTeardownReceiveMessage (connection, receiveMessageHandler, (void *)context);
	dbus_connection_unref (connection);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("DBUS TESTS\n");
	printf ("==========\n\n");

	init (argc, argv);

	// Test if dbus is available
	test_prerequisites ();

	// Test added, changed & deleted
	test_keyAdded ();
	test_keyChanged ();
	test_keyDeleted ();

	test_announceOnce ();

	test_cascadedAnnounceOnce ();
	test_cascadedChangeNotification ();

	printf ("\ntestmod_dbus RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	dbus_shutdown ();

	return nbError;
}
