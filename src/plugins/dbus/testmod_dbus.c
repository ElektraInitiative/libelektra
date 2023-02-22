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

#include <kdbchangetracking.h>

typedef struct
{
	char * lookupSignalName;
	char * receivedKeyName;

	DBusConnection * connection;
	int stop;
} TestContext;

#define TEST_TIMEOUT 1
#define TEST_DISPATCH_TIMEOUT 200

/** D-Bus bus type used by tests  */
DBusBusType testBusType;

/** key namespace to use for tests */
char * testKeyNamespace;

KeySet * oldKeys = NULL;
ChangeTrackingContext * changeTrackingContext = NULL;

const ChangeTrackingContext * elektraChangeTrackingGetContextFromKdb (ELEKTRA_UNUSED KDB * kdb)
{
	if (changeTrackingContext != NULL)
	{
		elektraChangeTrackingContextDel (changeTrackingContext);
	}

	changeTrackingContext = elektraChangeTrackingCreateContextForTesting (oldKeys);
	return changeTrackingContext;
}

/**
 * @internal
 * Process D-Bus messages and check for expected message.
 *
 * @param  connection D-Bus connection
 * @param  message    received D-Bus message
 * @param  data       test context
 * @return            message handler result
 */
static DBusHandlerResult receiveMessageHandler (DBusConnection * connection ELEKTRA_UNUSED, DBusMessage * message, void * data)
{
	TestContext * context = (TestContext *) data;

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

/**
 * @internal
 * Dispatch messages and declares timeout if dispatching is not stopped within
 * TEST_TIMEOUT.
 *
 * @param context test context
 */
static void runDispatch (TestContext * context)
{
	time_t now;
	time_t start = time (NULL);
	context->stop = 0;
	while (!context->stop && dbus_connection_read_write_dispatch (context->connection, TEST_DISPATCH_TIMEOUT))
	{
		now = time (NULL);
		// Stop dispatching after one second
		if (now - start > TEST_TIMEOUT)
		{
			succeed_if (0, "timeout exceeded; test failed");
			break;
		}
	}
}

/**
 * @internal
 * Create new test context.
 *
 * @param  connection D-Bus connection
 * @param  signalName Expected signal name
 * @return            Context
 */
static TestContext * createTestContext (DBusConnection * connection, char * signalName)
{
	TestContext * context = elektraMalloc (sizeof *context);
	exit_if_fail (context, "malloc failed");

	context->lookupSignalName = signalName;
	context->connection = connection;
	context->receivedKeyName = "";
	return context;
}

/**
 * @internal
 * Get and setup D-Bus connection.
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
		dbus_shutdown ();
		return NULL;
	}
	dbus_error_free (&error);

	dbus_connection_set_exit_on_disconnect (connection, FALSE);

	return connection;
}

static int test_prerequisites (void)
{
	printf ("testing prerequisites\n");
	printf ("detecting available bus types - please ignore single error messages prefixed with \"connect:\"\n");

	// Set bus type for tests
	// NOTE brew dbus on MacOs supports session out of the box while session
	// bus is not available without further configuration on Linux

	DBusConnection * systemBus = getDbusConnection (DBUS_BUS_SYSTEM);
	if (systemBus)
	{
		testBusType = DBUS_BUS_SYSTEM;
		testKeyNamespace = "system:/";
		dbus_connection_unref (systemBus);
		return 1; // success
	}

	DBusConnection * sessionBus = getDbusConnection (DBUS_BUS_SESSION);
	if (sessionBus)
	{
		testBusType = DBUS_BUS_SESSION;
		testKeyNamespace = "user:/";
		dbus_connection_unref (sessionBus);
		return 1; // success
	}

	return 0;
}

static void test_keyAdded (void)
{
	printf ("test adding keys\n");

	ksClear (oldKeys);

	// (namespace)/tests/foo
	Key * parentKey = keyNew (testKeyNamespace, KEY_END);
	keyAddName (parentKey, "tests/foo");

	// (namespace)/tests/foo/bar
	Key * toAdd = keyDup (parentKey, KEY_CP_ALL);
	keyAddName (toAdd, "bar");
	keySetString (toAdd, "test");

	KeySet * ks = ksNew (0, KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("dbus");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// add key to keyset
	ksAppendKey (ks, toAdd);

	DBusConnection * connection = getDbusConnection (testBusType);
	TestContext * context = createTestContext (connection, "KeyAdded");
	elektraDbusSetupReceiveMessage (connection, receiveMessageHandler, (void *) context);

	plugin->kdbCommit (plugin, ks, parentKey);
	runDispatch (context);

	succeed_if_same_string (keyName (toAdd), context->receivedKeyName);

	elektraFree (context);
	elektraDbusTeardownReceiveMessage (connection, receiveMessageHandler, (void *) context);
	dbus_connection_unref (connection);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_keyChanged (void)
{
	printf ("test changing keys\n");

	ksClear (oldKeys);

	// All keys created by keyNew have the KEY_FLAG_SYNC set and will be
	// detected as changed by the dbus plugin
	// This flag is only cleared after kdbCommit or when keys come from a backend.

	// (namespace)/tests/foo
	Key * parentKey = keyNew (testKeyNamespace, KEY_END);
	keyAddName (parentKey, "tests/foo");

	// (namespace)/tests/foo/bar
	Key * toChange = keyDup (parentKey, KEY_CP_ALL);
	keyAddName (toChange, "bar");
	keySetString (toChange, "test");

	KeySet * ks = ksNew (2, toChange, KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("dbus");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);
	ksAppendKey(oldKeys, keyDup (toChange, KEY_CP_ALL));

	// change key in keyset
	keySetString (toChange, "new value");

	DBusConnection * connection = getDbusConnection (testBusType);
	TestContext * context = createTestContext (connection, "KeyChanged");
	elektraDbusSetupReceiveMessage (connection, receiveMessageHandler, (void *) context);

	plugin->kdbCommit (plugin, ks, parentKey);
	runDispatch (context);

	succeed_if_same_string (keyName (toChange), context->receivedKeyName);

	elektraFree (context);
	elektraDbusTeardownReceiveMessage (connection, receiveMessageHandler, (void *) context);
	dbus_connection_unref (connection);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_keyDeleted (void)
{
	printf ("test deleting keys\n");

	ksClear (oldKeys);

	// (namespace)/tests/foo
	Key * parentKey = keyNew (testKeyNamespace, KEY_END);
	keyAddName (parentKey, "tests/foo");

	// (namespace)/tests/foo/bar
	Key * toDelete = keyDup (parentKey, KEY_CP_ALL);
	keyAddName (toDelete, "bar");
	keySetString (toDelete, "test");

	KeySet * ks = ksNew (1, keyDup (toDelete, KEY_CP_ALL), KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("dbus");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);
	ksAppend (oldKeys, ks);

	// remove key from keyset
	Key * deleted = ksLookup (ks, toDelete, KDB_O_POP);
	succeed_if (deleted != NULL, "key was not found");

	DBusConnection * connection = getDbusConnection (testBusType);
	TestContext * context = createTestContext (connection, "KeyDeleted");
	elektraDbusSetupReceiveMessage (connection, receiveMessageHandler, (void *) context);

	plugin->kdbCommit (plugin, ks, parentKey);
	runDispatch (context);

	succeed_if_same_string (keyName (toDelete), context->receivedKeyName);

	elektraFree (context);
	elektraDbusTeardownReceiveMessage (connection, receiveMessageHandler, (void *) context);
	dbus_connection_unref (connection);
	keyDel (toDelete);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_announceOnce (void)
{
	printf ("test announce once\n");

	ksClear (oldKeys);

	// (namespace)/tests/foo
	Key * parentKey = keyNew (testKeyNamespace, KEY_END);
	keyAddName (parentKey, "tests/foo");

	// (namespace)/tests/foo/bar/#0
	Key * toAdd1 = keyDup (parentKey, KEY_CP_ALL);
	keyAddName (toAdd1, "bar/#0");
	keySetString (toAdd1, "test");

	// (namespace)/tests/foo/bar/#1
	Key * toAdd2 = keyDup (toAdd1, KEY_CP_ALL);
	keySetBaseName (toAdd2, "#1");

	// (namespace)/tests/foo/bar
	Key * toChange = keyDup (parentKey, KEY_CP_ALL);
	keyAddName (toChange, "bar");
	keySetString (toChange, "test");

	KeySet * ks = ksNew (1, toChange, KS_END);

	KeySet * conf = ksNew (1, keyNew ("/announce", KEY_VALUE, "once", KEY_END), KS_END);
	PLUGIN_OPEN ("dbus");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);
	ksAppend (oldKeys, ks);

	// modify keyset
	ksAppendKey (ks, toAdd1);
	ksAppendKey (ks, toAdd2);
	keySetString (toChange, "new value");

	DBusConnection * connection = getDbusConnection (testBusType);
	TestContext * context = createTestContext (connection, "Commit");
	elektraDbusSetupReceiveMessage (connection, receiveMessageHandler, (void *) context);

	plugin->kdbCommit (plugin, ks, parentKey);
	runDispatch (context);

	succeed_if_same_string (keyName (parentKey), context->receivedKeyName);

	elektraFree (context);
	elektraDbusTeardownReceiveMessage (connection, receiveMessageHandler, (void *) context);
	dbus_connection_unref (connection);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_cascadedChangeNotification (void)
{
	printf ("test change notification with cascaded parent key\n");

	ksClear (oldKeys);

	Key * parentKey = keyNew ("/tests/foo", KEY_END);

	// (namespace)/tests/foo
	Key * completeParentKey = keyNew (testKeyNamespace, KEY_END);
	keyAddName (completeParentKey, "tests/foo");

	// (namespace)/tests/foo/bar
	Key * toAdd = keyDup (completeParentKey, KEY_CP_ALL);
	keyAddName (toAdd, "bar");
	keySetString (toAdd, "test");

	KeySet * ks = ksNew (1, completeParentKey, KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("dbus");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);
	ksAppend (oldKeys, ks);

	// add key to keyset
	ksAppendKey (ks, toAdd);

	DBusConnection * connection = getDbusConnection (testBusType);
	TestContext * context = createTestContext (connection, "KeyAdded");
	elektraDbusSetupReceiveMessage (connection, receiveMessageHandler, (void *) context);

	plugin->kdbCommit (plugin, ks, parentKey);
	runDispatch (context);

	succeed_if_same_string (keyName (toAdd), context->receivedKeyName);

	elektraFree (context);
	elektraDbusTeardownReceiveMessage (connection, receiveMessageHandler, (void *) context);
	dbus_connection_unref (connection);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_cascadedAnnounceOnce (void)
{
	printf ("test announce once with cascaded parent key\n");

	ksClear (oldKeys);

	Key * parentKey = keyNew ("/tests/foo", KEY_END);

	// (namespace)/tests/foo
	Key * completeParentKey = keyNew (testKeyNamespace, KEY_END);
	keyAddName (completeParentKey, "tests/foo");

	// (namespace)/tests/foo/bar
	Key * toAdd = keyDup (completeParentKey, KEY_CP_ALL);
	keyAddName (toAdd, "bar");
	keySetString (toAdd, "test");

	KeySet * ks = ksNew (1, completeParentKey, KS_END);

	KeySet * conf = ksNew (1, keyNew ("/announce", KEY_VALUE, "once", KEY_END), KS_END);
	PLUGIN_OPEN ("dbus");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);
	ksAppend (oldKeys, ks);

	// add key to keyset
	ksAppendKey (ks, toAdd);

	DBusConnection * connection = getDbusConnection (testBusType);
	TestContext * context = createTestContext (connection, "Commit");
	elektraDbusSetupReceiveMessage (connection, receiveMessageHandler, (void *) context);

	plugin->kdbCommit (plugin, ks, parentKey);
	runDispatch (context);

	succeed_if_same_string (keyName (completeParentKey), context->receivedKeyName);

	elektraFree (context);
	elektraDbusTeardownReceiveMessage (connection, receiveMessageHandler, (void *) context);
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

	oldKeys = ksNew (0, KS_END);
	ksIncRef (oldKeys);

	// Test if dbus is available
	if (test_prerequisites ())
	{
		// Test added, changed & deleted
		test_keyAdded ();
		test_keyChanged ();
		test_keyDeleted ();

		test_announceOnce ();

		test_cascadedAnnounceOnce ();
		test_cascadedChangeNotification ();
	}
	else
	{
		printf ("warning no dbus daemon available; skipping tests that require dbus\n");
	}

	print_result ("testmod_dbus");

	dbus_shutdown ();

	if (changeTrackingContext != NULL)
	{
		elektraChangeTrackingContextDel (changeTrackingContext);
	}

	ksDecRef (oldKeys);
	ksDel (oldKeys);

	return nbError;
}
