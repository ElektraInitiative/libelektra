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
#include <kdbnotificationinternal.h>

#include <tests.h>
#include <tests_plugin.h>

#include "internalnotification.h"

int callback_called;
char * callback_keyValue;
char * callback_keyName;

#define CALLBACK_CONTEXT_MAGIC_NUMBER ((void *) 1234)

static int internalnotificationRegisterInt (Plugin * plugin, Key * key, int * variable)
{
	size_t address = elektraPluginGetFunction (plugin, "registerInt");

	// Register key with plugin
	return ((ElektraNotificationPluginRegisterInt) address) (plugin, key, variable);
}

static int internalnotificationRegisterFloat (Plugin * plugin, Key * key, float * variable)
{
	size_t address = elektraPluginGetFunction (plugin, "registerFloat");

	// Register key with plugin
	return ((ElektraNotificationPluginRegisterFloat) address) (plugin, key, variable);
}

static int internalnotificationRegisterCallback (Plugin * plugin, Key * key, ElektraNotificationChangeCallback callback, void * context)
{
	size_t address = elektraPluginGetFunction (plugin, "registerCallback");

	// Register key with plugin
	return ((ElektraNotificationPluginRegisterCallback) address) (plugin, key, callback, context);
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


static void test_floatUpdateWithCascadingKey (void)
{
	printf ("test update with cascading key registered\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * registeredKey = keyNew ("/test/internalnotification/value", KEY_END);
	float value = 0;
	succeed_if (internalnotificationRegisterFloat (plugin, registeredKey, &value) == 1,
		    "call to elektraInternalnotificationRegisterFloat was not successful");

	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_VALUE, "2.3", KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);

	succeed_if (value >= 2.295 && value <= 2.305, "registered value was not updated");

	keyDel (registeredKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_floatNoUpdateWithInvalidValue (void)
{
	printf ("test no update with invalid value\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	float value = 0.0;
	succeed_if (internalnotificationRegisterFloat (plugin, valueKey, &value) == 1,
		    "call to elektraInternalnotificationRegisterFloat was not successful");

	keySetString (valueKey, "4.a");


	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);

	succeed_if ((int) value == 0, "registered value was updated");

	ksDel (ks);
	PLUGIN_CLOSE ();
}


static void test_callback (Key * key, void * context)
{
	succeed_if (context == CALLBACK_CONTEXT_MAGIC_NUMBER, "callback context was not passed");
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

	succeed_if (internalnotificationRegisterCallback (plugin, valueKey, test_callback, CALLBACK_CONTEXT_MAGIC_NUMBER) == 1,
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

	succeed_if (internalnotificationRegisterCallback (plugin, valueKey, test_callback, CALLBACK_CONTEXT_MAGIC_NUMBER) == 1,
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

	printf ("\nregisterFloat\n-----------\n");
	test_floatUpdateWithCascadingKey ();
	test_floatNoUpdateWithInvalidValue ();

	printf ("\nregisterCallback\n----------------\n");
	test_callbackCalledWithKey ();
	test_callbackCalledWithChangeDetection ();

	printf ("\ntestmod_internalnotification RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
