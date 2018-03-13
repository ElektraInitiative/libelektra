/**
 * @file
 *
 * @brief Tests for internalnotification plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <math.h>
#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>
#include <kdbnotificationplugin.h>

#include <tests.h>
#include <tests_plugin.h>

#include "internalnotification.h"

int callback_called;
char * callback_keyValue;
char * callback_keyName;

/**
 * @internal
 * Retrieves a function exported by a plugin.
 *
 * @param  plugin Plugin handle
 * @param  name   Function name
 * @return        Pointer to function
 */
static size_t getPluginFunction (Plugin * plugin, const char * name)
{
	KeySet * exports = ksNew (0, KS_END);
	Key * pk = keyNew ("system/elektra/modules", KEY_END);
	keyAddBaseName (pk, plugin->name);
	plugin->kdbGet (plugin, exports, pk);
	ksRewind (exports);
	keyAddBaseName (pk, "exports");
	keyAddBaseName (pk, name);
	Key * keyFunction = ksLookup (exports, pk, 0);

	size_t * buffer;
	size_t bufferSize = keyGetValueSize (keyFunction);
	buffer = elektraMalloc (bufferSize);
	if (buffer)
	{
		int result = keyGetBinary (keyFunction, buffer, bufferSize);
		if (result == -1 || buffer == NULL)
		{
			ELEKTRA_LOG_WARNING ("could not get function \"%s\" from plugin \"%s\"", name, plugin->name);
			return 0;
		}
	}

	size_t func = *buffer;

	elektraFree (buffer);
	ksDel (exports);
	keyDel (pk);

	return func;
}

static int internalnotificationRegisterInt (Plugin * plugin, Key * key, int * variable)
{
	size_t address = getPluginFunction (plugin, "registerInt");

	// Register key with plugin
	return ((ElektraNotificationPluginRegisterInt) address) (plugin, key, variable);
}

static int internalnotificationRegisterCallback (Plugin * plugin, Key * key, ElektraNotificationChangeCallback callback)
{
	size_t address = getPluginFunction (plugin, "registerCallback");

	// Register key with plugin
	return ((ElektraNotificationPluginRegisterCallback) address) (plugin, key, callback);
}

static int digits (long long number)
{
	int digits = 0;
	while (number)
	{
		number /= 10;
		digits++;
	}
	return digits;
}

static char * convertLongLongToString (long long number)
{
	int correction = 1; // Allocate space for '\0'
	int invert = 1;
	if (number < 0)
	{
		invert = -1;     // Invert negative numbers
		correction += 1; // Allocate extra space for sign ('-')
	}
	int size = digits (number * invert) + correction;

	char * buffer = elektraMalloc (size);
	exit_if_fail (buffer != NULL, "elektraMalloc failed!");

	sprintf (buffer, "%lli", number);
	succeed_if (buffer[0] != '0', "number conversion failed!");

	return buffer;
}

static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew ("user/tests/internalnotification", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == 1, "call to kdbOpen was not successful");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");

	succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "call to kdbClose was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_updateOnKdbGet (void)
{
	printf ("test update on kdbGet\n");

	Key * parentKey = keyNew ("user/tests/internalnotification", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_VALUE, "42", KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	int value = 0;
	succeed_if (internalnotificationRegisterInt (plugin, valueKey, &value) == 1,
		    "call to elektraInternalnotificationRegisterInt was not successful");

	plugin->kdbGet (plugin, ks, parentKey);

	succeed_if (value == 42, "registered value was not updated");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_updateOnKdbSet (void)
{
	printf ("test update on kdbSet\n");

	Key * parentKey = keyNew ("user/tests/internalnotification", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_VALUE, "42", KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	int value = 0;
	succeed_if (internalnotificationRegisterInt (plugin, valueKey, &value) == 1,
		    "call to elektraInternalnotificationRegisterInt was not successful");

	plugin->kdbSet (plugin, ks, parentKey);

	succeed_if (value == 42, "registered value was not updated");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_intUpdateWithCascadingKey (void)
{
	printf ("test update with cascading key registered\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * registeredKey = keyNew ("/test/internalnotification/value", KEY_END);
	int value = 0;
	succeed_if (internalnotificationRegisterInt (plugin, registeredKey, &value) == 1,
		    "call to elektraInternalnotificationRegisterInt was not successful");

	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_VALUE, "42", KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);

	succeed_if (value == 42, "registered value was not updated");

	keyDel (registeredKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_intNoUpdateWithInvalidValue (void)
{
	printf ("test no update with invalid value\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	int value = 123;
	succeed_if (internalnotificationRegisterInt (plugin, valueKey, &value) == 1,
		    "call to elektraInternalnotificationRegisterInt was not successful");

	keySetString (valueKey, "42abcd");


	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);

	succeed_if (value == 123, "registered value was updated");

	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_intUpdateWithValueNotYetExceedingIntMax (void)
{
	printf ("test update with value = INT_MAX\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	int value = 123;
	succeed_if (internalnotificationRegisterInt (plugin, valueKey, &value) == 1,
		    "call to elektraInternalnotificationRegisterInt was not successful");

	int exceedsInt = INT_MAX;
	char * stringValue = convertLongLongToString ((long long) exceedsInt);
	keySetString (valueKey, stringValue);


	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);

	succeed_if (value == INT_MAX, "registered value was not updated");

	elektraFree (stringValue);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_intNoUpdateWithValueExceedingIntMax (void)
{
	printf ("test no update with value that exceeds INT_MAX\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	int value = 123;
	succeed_if (internalnotificationRegisterInt (plugin, valueKey, &value) == 1,
		    "call to elektraInternalnotificationRegisterInt was not successful");

	long long exceedsInt = (long long) INT_MAX + 1;
	char * stringValue = convertLongLongToString (exceedsInt);
	keySetString (valueKey, stringValue);


	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);

	succeed_if (value == 123, "registered value was updated");

	elektraFree (stringValue);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


static void test_intUpdateWithValueNotYetExceedingIntMin (void)
{
	printf ("test update with value = INT_MIN\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	int value = 123;
	succeed_if (internalnotificationRegisterInt (plugin, valueKey, &value) == 1,
		    "call to elektraInternalnotificationRegisterInt was not successful");

	int exceedsInt = INT_MIN;
	char * stringValue = convertLongLongToString ((long long) exceedsInt);
	keySetString (valueKey, stringValue);


	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);

	succeed_if (value == INT_MIN, "registered value was not updated");

	elektraFree (stringValue);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_intNoUpdateWithValueExceedingIntMin (void)
{
	printf ("test no update with value that exceeds INT_MIN\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	int value = 123;
	succeed_if (internalnotificationRegisterInt (plugin, valueKey, &value) == 1,
		    "call to elektraInternalnotificationRegisterInt was not successful");

	long long exceedsInt = (long long) INT_MIN - 1;
	char * stringValue = convertLongLongToString (exceedsInt);
	keySetString (valueKey, stringValue);

	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);

	succeed_if (value == 123, "registered value was updated");

	elektraFree (stringValue);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_callback (Key * key)
{
	callback_called = 1;
	callback_keyValue = (char *) keyValue (key);
	callback_keyName = (char *) keyName (key);
}

static void test_callbackCalledWithKey (void)
{
	printf ("test callback is called with changed key\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	char * value = "foobaroo!";
	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_VALUE, value, KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	succeed_if (internalnotificationRegisterCallback (plugin, valueKey, test_callback) == 1,
		    "call to elektraInternalnotificationRegisterCallback was not successful");

	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);

	succeed_if (callback_called, "registered value was not updated");
	succeed_if_same_string (callback_keyName, keyName (valueKey));
	succeed_if_same_string (callback_keyValue, value);

	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_callbackCalledWithChangeDetection (void)
{
	printf ("test callback is not called when key has not changed\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	char * value = "foobaroo!";
	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_VALUE, value, KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	succeed_if (internalnotificationRegisterCallback (plugin, valueKey, test_callback) == 1,
		    "call to elektraInternalnotificationRegisterCallback was not successful");

	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);

	succeed_if (callback_called, "registered value was not updated");

	callback_called = 0;
	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);
	succeed_if (callback_called == 0, "registered value was updated but value has not changed");

	ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("INTERNALNOTIFICATION     TESTS\n");
	printf ("==============================\n\n");

	init (argc, argv);

	test_basics ();
	test_updateOnKdbGet ();
	test_updateOnKdbSet ();

	printf ("\nregisterInt\n-----------\n");
	test_intUpdateWithCascadingKey ();
	test_intNoUpdateWithInvalidValue ();
	test_intUpdateWithValueNotYetExceedingIntMax ();
	test_intNoUpdateWithValueExceedingIntMax ();
	test_intUpdateWithValueNotYetExceedingIntMin ();
	test_intNoUpdateWithValueExceedingIntMin ();

	printf ("\nregisterCallback\n----------------\n");
	test_callbackCalledWithKey ();
	test_callbackCalledWithChangeDetection ();

	printf ("\ntestmod_internalnotification RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
