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
#include <kdbtypes.h>

#include <tests.h>
#include <tests_plugin.h>

#include "internalnotification.h"

int callback_called;
char * callback_keyValue;
char * callback_keyName;

int doUpdate_callback_called;

#define CALLBACK_CONTEXT_MAGIC_NUMBER ((void *) 1234)

#define REGISTER_FUNC_NAME(TYPE_NAME) internalnotificationRegister##TYPE_NAME
#define TEST_CASE_UPDATE_NAME(TYPE_NAME) test_update##TYPE_NAME
#define TEST_CASE_NO_UPDATE_NAME(TYPE_NAME) test_noUpdate##TYPE_NAME

#define RUN_TYPE_TESTS(TYPE_NAME)                                                                                                          \
	printf ("\n" #TYPE_NAME "\n----------------\n");                                                                                   \
	TEST_CASE_UPDATE_NAME (TYPE_NAME) ();                                                                                              \
	TEST_CASE_NO_UPDATE_NAME (TYPE_NAME) ();

#define REGISTER_FUNC_DEFINITION(TYPE, TYPE_NAME)                                                                                          \
	static int REGISTER_FUNC_NAME (TYPE_NAME) (Plugin * plugin, Key * key, TYPE * variable)                                            \
	{                                                                                                                                  \
		size_t address = elektraPluginGetFunction (plugin, "register" #TYPE_NAME);                                                 \
		if (!address) yield_error ("function not exported");                                                                       \
                                                                                                                                           \
		/* register key with plugin */                                                                                             \
		ELEKTRA_NOTIFICATION_REGISTERFUNC_TYPEDEF (RegisterFuncType, TYPE)                                                         \
		return ((RegisterFuncType) address) (plugin, key, variable);                                                               \
	}

#define CREATE_UPDATE_TEST_CASE(TYPE, TYPE_NAME, FORMAT_STRING, TEST_VALUE, CHECK_VALUE)                                                   \
	static void TEST_CASE_UPDATE_NAME (TYPE_NAME) (void)                                                                               \
	{                                                                                                                                  \
		printf (#TYPE ": test update\n");                                                                                          \
		KeySet * conf = ksNew (0, KS_END);                                                                                         \
		PLUGIN_OPEN ("internalnotification");                                                                                      \
		Key * registeredKey = keyNew ("/test/internalnotification/value", KEY_END);                                                \
		TYPE value = 0;                                                                                                            \
		succeed_if (REGISTER_FUNC_NAME (TYPE_NAME) (plugin, registeredKey, &value) == 1, "registration was not successful");       \
		char * valueStr = elektraFormat (FORMAT_STRING, TEST_VALUE);                                                               \
		Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_VALUE, valueStr, KEY_END);                            \
		KeySet * ks = ksNew (1, valueKey, KS_END);                                                                                 \
		elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);                                                              \
		succeed_if (CHECK_VALUE, "registered value was not updated");                                                              \
		free (valueStr);                                                                                                           \
		keyDel (registeredKey);                                                                                                    \
		ksDel (ks);                                                                                                                \
		PLUGIN_CLOSE ();                                                                                                           \
	}

#define CREATE_TYPE_TESTS(TYPE, TYPE_NAME, FORMAT_STRING, TEST_VALUE, CHECK_VALUE, INVALID_VALUE, CHECK_INVALID)                           \
	REGISTER_FUNC_DEFINITION (TYPE, TYPE_NAME)                                                                                         \
                                                                                                                                           \
	CREATE_UPDATE_TEST_CASE (TYPE, TYPE_NAME, FORMAT_STRING, TEST_VALUE, CHECK_VALUE)                                                  \
                                                                                                                                           \
	static void TEST_CASE_NO_UPDATE_NAME (TYPE_NAME) (void)                                                                            \
	{                                                                                                                                  \
		printf (#TYPE ": test no update with invalid value\n");                                                                    \
		KeySet * conf = ksNew (0, KS_END);                                                                                         \
		PLUGIN_OPEN ("internalnotification");                                                                                      \
		Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);                                                 \
		KeySet * ks = ksNew (1, valueKey, KS_END);                                                                                 \
		TYPE value = 0;                                                                                                            \
		succeed_if (REGISTER_FUNC_NAME (TYPE_NAME) (plugin, valueKey, &value) == 1, "registration was not successful");            \
		keySetString (valueKey, INVALID_VALUE);                                                                                    \
		elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);                                                              \
		succeed_if (CHECK_INVALID, "registered value was updated");                                                                \
		ksDel (ks);                                                                                                                \
		PLUGIN_CLOSE ();                                                                                                           \
	}


static int internalnotificationRegisterInt (Plugin * plugin, Key * key, int * variable)
{
	size_t address = elektraPluginGetFunction (plugin, "registerInt");
	if (!address) yield_error ("function not exported");

	// Register key with plugin
	ELEKTRA_NOTIFICATION_REGISTERFUNC_TYPEDEF (RegisterFuncType, int)
	return ((RegisterFuncType) address) (plugin, key, variable);
}

static int internalnotificationRegisterCallback (Plugin * plugin, Key * key, ElektraNotificationChangeCallback callback, void * context)
{
	size_t address = elektraPluginGetFunction (plugin, "registerCallback");
	if (!address) yield_error ("function not exported");

	// Register key with plugin
	return ((ElektraNotificationPluginRegisterCallback) address) (plugin, key, callback, context);
}

static int internalnotificationRegisterCallbackSameOrBelow (Plugin * plugin, Key * key, ElektraNotificationChangeCallback callback,
							    void * context)
{
	size_t address = elektraPluginGetFunction (plugin, "registerCallbackSameOrBelow");
	if (!address) yield_error ("function not exported");

	// Register key with plugin
	return ((ElektraNotificationPluginRegisterCallbackSameOrBelow) address) (plugin, key, callback, context);
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

static void test_doUpdate_callback (KDB * kdb ELEKTRA_UNUSED, Key * changedKey ELEKTRA_UNUSED)
{
	doUpdate_callback_called = 1;
}

static void test_doUpdateShouldUpdateKey (void)
{
	printf ("test doUpdate should update same key\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * changedKey = keyNew ("user/test/internalnotification/value", KEY_END);

	succeed_if (internalnotificationRegisterCallback (plugin, changedKey, test_callback, NULL) == 1,
		    "call to elektraInternalnotificationRegisterCallback was not successful");

	ElektraNotificationCallbackContext * context = elektraMalloc (sizeof *context);
	context->kdbUpdate = NULL;
	context->kdbUpdate = test_doUpdate_callback;
	context->notificationPlugin = plugin;

	doUpdate_callback_called = 0;
	elektraInternalnotificationDoUpdate (changedKey, context);

	succeed_if (doUpdate_callback_called, "did not call callback for registered key");

	elektraFree (context);
	PLUGIN_CLOSE ();
}

static void test_doUpdateShouldUpdateKeyBelow (void)
{
	printf ("test doUpdate should update key below changed key\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * changedKey = keyNew ("user/test/internalnotification", KEY_END);

	Key * registeredKey = keyNew ("user/test/internalnotification/value", KEY_END);
	succeed_if (internalnotificationRegisterCallback (plugin, registeredKey, test_callback, NULL) == 1,
		    "call to elektraInternalnotificationRegisterCallback was not successful");

	ElektraNotificationCallbackContext * context = elektraMalloc (sizeof *context);
	context->kdbUpdate = NULL;
	context->kdbUpdate = test_doUpdate_callback;
	context->notificationPlugin = plugin;

	doUpdate_callback_called = 0;
	elektraInternalnotificationDoUpdate (changedKey, context);

	succeed_if (doUpdate_callback_called, "did not call callback for registered key");

	elektraFree (context);
	keyDel (registeredKey);
	PLUGIN_CLOSE ();
}

static void test_doUpdateShouldNotUpdateKeyAbove (void)
{
	printf ("test doUpdate should not update key above changed key\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * changedKey = keyNew ("user/test/internalnotification/value", KEY_END);

	Key * registeredKey = keyNew ("user/test/internalnotification", KEY_END);
	succeed_if (internalnotificationRegisterCallback (plugin, registeredKey, test_callback, NULL) == 1,
		    "call to elektraInternalnotificationRegisterCallback was not successful");

	ElektraNotificationCallbackContext * context = elektraMalloc (sizeof *context);
	context->kdbUpdate = NULL;
	context->kdbUpdate = test_doUpdate_callback;
	context->notificationPlugin = plugin;

	doUpdate_callback_called = 0;
	elektraInternalnotificationDoUpdate (changedKey, context);

	succeed_if (doUpdate_callback_called == 0, "did call callback for key above");

	elektraFree (context);
	keyDel (registeredKey);
	PLUGIN_CLOSE ();
}

static void test_doUpdateShouldUpdateKeyAbove (void)
{
	printf ("test doUpdate should update key above changed key for sameOrBelow callbacks\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * changedKey = keyNew ("user/test/internalnotification/value", KEY_END);

	Key * registeredKey = keyNew ("user/test/internalnotification", KEY_END);
	succeed_if (internalnotificationRegisterCallbackSameOrBelow (plugin, registeredKey, test_callback, NULL) == 1,
		    "call to internalnotificationRegisterCallbackSameOrBelow was not successful");

	ElektraNotificationCallbackContext * context = elektraMalloc (sizeof *context);
	context->kdbUpdate = NULL;
	context->kdbUpdate = test_doUpdate_callback;
	context->notificationPlugin = plugin;

	doUpdate_callback_called = 0;
	elektraInternalnotificationDoUpdate (changedKey, context);

	succeed_if (doUpdate_callback_called, "did not call callback for key above");

	elektraFree (context);
	keyDel (registeredKey);
	PLUGIN_CLOSE ();
}

static void test_doUpdateShouldNotUpdateUnregisteredKey (void)
{
	printf ("test doUpdate should not update unregistered key\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * changedKey = keyNew ("user/test/internalnotification/value", KEY_END);

	// No key registration made

	ElektraNotificationCallbackContext * context = elektraMalloc (sizeof *context);
	context->kdbUpdate = NULL;
	context->kdbUpdate = test_doUpdate_callback;
	context->notificationPlugin = plugin;

	doUpdate_callback_called = 0;
	elektraInternalnotificationDoUpdate (changedKey, context);

	succeed_if (doUpdate_callback_called == 0, "did call callback for unregistered key");

	elektraFree (context);
	PLUGIN_CLOSE ();
}

CREATE_TYPE_TESTS (unsigned int, UnsignedInt, "%u", UINT_MAX, (value == UINT_MAX), "-1", value == 0)
CREATE_TYPE_TESTS (long, Long, "%ld", LONG_MAX, (value == LONG_MAX), "5000abc000", value == 0)
CREATE_TYPE_TESTS (unsigned long, UnsignedLong, "%lu", ULONG_MAX, (value == ULONG_MAX), "AA446744073709551615", value == 0)

CREATE_TYPE_TESTS (float, Float, "%f", 2.3, (value >= 2.295 && value <= 2.305), "4.a", ((int) value == 0))
CREATE_TYPE_TESTS (double, Double, "%1.8f", 1.00000001, (value >= 1 + 1e-9 && value <= 1 + 1e-7), "4.a", ((int) value == 0))

REGISTER_FUNC_DEFINITION (kdb_boolean_t, KdbBoolean)
CREATE_UPDATE_TEST_CASE (kdb_boolean_t, KdbBoolean, "%d", 1, (value))

REGISTER_FUNC_DEFINITION (kdb_char_t, KdbChar)
CREATE_UPDATE_TEST_CASE (kdb_char_t, KdbChar, "abc%d", 1, (value == 'a'))

CREATE_TYPE_TESTS (kdb_octet_t, KdbOctet, "%d", 255, (value == 255), "4a", value == 0)
CREATE_TYPE_TESTS (kdb_short_t, KdbShort, "%d", SHRT_MIN, (value == SHRT_MIN), "-55ABC", value == 0)
CREATE_TYPE_TESTS (kdb_unsigned_short_t, KdbUnsignedShort, "%d", USHRT_MAX, (value == USHRT_MAX), "55ABC", value == 0)
CREATE_TYPE_TESTS (kdb_long_t, KdbLong, "%d", INT_MIN, (value == INT_MIN), "B5C", value == 0)
CREATE_TYPE_TESTS (kdb_unsigned_long_t, KdbUnsignedLong, "%d", UINT_MAX, (value == UINT_MAX), "B5C", value == 0)
CREATE_TYPE_TESTS (kdb_long_long_t, KdbLongLong, ELEKTRA_LONG_LONG_F, LONG_MIN, (value == LONG_MIN), "50000asasd", value == 0)
CREATE_TYPE_TESTS (kdb_unsigned_long_long_t, KdbUnsignedLongLong, ELEKTRA_UNSIGNED_LONG_LONG_F, ULONG_MAX, (value == ULONG_MAX), "-B5C",
		   value == 0)
CREATE_TYPE_TESTS (kdb_float_t, KdbFloat, "%f", 2.3, (value >= 2.295 && value <= 2.305), "4.a", ((int) value == 0))
CREATE_TYPE_TESTS (kdb_double_t, KdbDouble, "%1.8f", 1.00000001, (value >= 1 + 1e-9 && value <= 1 + 1e-7), "4.a", ((int) value == 0))
CREATE_TYPE_TESTS (kdb_long_double_t, KdbLongDouble, "%1.8f", 1.00000001, (value >= 1 + 1e-9 && value <= 1 + 1e-7), "4.a",
		   ((int) value == 0))

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

	RUN_TYPE_TESTS (UnsignedInt)
	RUN_TYPE_TESTS (Long)
	RUN_TYPE_TESTS (UnsignedLong)

	RUN_TYPE_TESTS (Float)
	RUN_TYPE_TESTS (Double)

	printf ("\nKdbBoolean\n----------------\n");
	TEST_CASE_UPDATE_NAME (KdbBoolean) ();

	printf ("\nKdbChar\n----------------\n");
	TEST_CASE_UPDATE_NAME (KdbChar) ();

	RUN_TYPE_TESTS (KdbOctet)
	RUN_TYPE_TESTS (KdbShort)
	RUN_TYPE_TESTS (KdbUnsignedShort)
	RUN_TYPE_TESTS (KdbLong)
	RUN_TYPE_TESTS (KdbUnsignedLong)
	RUN_TYPE_TESTS (KdbLongLong)
	RUN_TYPE_TESTS (KdbUnsignedLongLong)
	RUN_TYPE_TESTS (KdbFloat)
	RUN_TYPE_TESTS (KdbDouble)
	RUN_TYPE_TESTS (KdbLongDouble)

	printf ("\nelektraInternalnotificationDoUpdate\n-----------------------------------\n");
	test_doUpdateShouldUpdateKey ();
	test_doUpdateShouldUpdateKeyBelow ();
	test_doUpdateShouldNotUpdateKeyAbove ();
	test_doUpdateShouldNotUpdateUnregisteredKey ();
	test_doUpdateShouldUpdateKeyAbove ();

	printf ("\ntestmod_internalnotification RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
