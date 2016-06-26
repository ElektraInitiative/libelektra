/**
 * @file
 *
 * @brief Tests for internalnotification plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <kdbconfig.h>

#include <tests_plugin.h>


static int internalnotificationRegisterInt (Plugin * plugin, int * variable, Key * key)
{
  typedef int (* elektraInternalnotificationRegisterIntCallback)(Plugin * handle, int * variable, Key * key);

  static size_t address = 0;

  if (address == 0)
  {
    char * NOTIFICATION_BASE = "system/elektra/modules/internalnotification";
    char * EXPORTED_FUNCTION = "system/elektra/modules/internalnotification/exports/elektraInternalnotificationRegisterInt";
    Key * parentKey = keyNew (
      NOTIFICATION_BASE,
      KEY_END
    );

    KeySet * conf = ksNew (20, KS_END);
    plugin->kdbGet (plugin, conf, parentKey);
    Key * keyFunction = ksLookupByName (conf,
      EXPORTED_FUNCTION,
      0);

    if (keyFunction == 0 || !keyIsBinary (keyFunction))
    {
      return -1;
    }

    size_t * buffer;
    size_t bufferSize = keyGetValueSize (keyFunction);
    buffer = elektraMalloc (bufferSize);
    if (buffer == NULL)
    {
      return -1;
    }
    if (keyGetBinary (keyFunction, buffer, bufferSize) == -1)
    {
      return -1;
    }

    // Verify that address is not null
    if (buffer == NULL)
    {
      return -1;
    }

    // Convert address from buffer
    address = * buffer;

    // Free allocated memory
    elektraFree (buffer);
    keyDel (parentKey);
    ksDel (conf);
  }

  // Register key with plugin
  return ((elektraInternalnotificationRegisterIntCallback)address)(plugin, variable, key);
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
    invert = -1;  // Invert negative numbers
    correction += 1; // Allocate extra space for sign ('-')
  }
  int size = digits (number * invert) + correction;

  char * buffer = elektraMalloc (size);
  exit_if_fail (buffer != NULL, "elektraMalloc failed!");

  sprintf (buffer, "%lli", number);
  succeed_if (buffer[0] != '0', "number conversion failed!");

  return buffer;
}

static void test_basics ()
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

static void test_updateOnKdbGet ()
{
	printf ("test update on kdbGet\n");

	Key * parentKey = keyNew ("user/tests/internalnotification", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);

	int value = 0;
	succeed_if (internalnotificationRegisterInt (plugin, &value, valueKey) == 1, "call to elektraInternalnotificationRegisterInt was not successful");

	keySetString (valueKey, "42");

	KeySet * ks = ksNew (1, KS_END);
	ksAppendKey (ks, valueKey);

	plugin->kdbGet (plugin, ks, parentKey);

	succeed_if (value == 42, "registered value was not updated");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_updateOnKdbSet ()
{
	printf ("test update on kdbSet\n");

	Key * parentKey = keyNew ("user/tests/internalnotification", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);

	int value = 0;
	succeed_if (internalnotificationRegisterInt (plugin, &value, valueKey) == 1, "call to elektraInternalnotificationRegisterInt was not successful");

	keySetString (valueKey, "42");

	KeySet * ks = ksNew (1, KS_END);
	ksAppendKey (ks, valueKey);

	plugin->kdbSet (plugin, ks, parentKey);

	succeed_if (value == 42, "registered value was not updated");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_updateWithCascadingKey ()
{
	printf ("test update with cascading key registered\n");

  Key * parentKey = keyNew ("user/tests/internalnotification", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

  Key * registeredKey = keyNew ("/test/internalnotification/value", KEY_END);

  int value = 0;
  succeed_if (internalnotificationRegisterInt (plugin, &value, registeredKey) == 1, "call to elektraInternalnotificationRegisterInt was not successful");

  Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);
	keySetString (valueKey, "42");

	KeySet * ks = ksNew (1, KS_END);
	ksAppendKey (ks, valueKey);

	plugin->kdbGet (plugin, ks, parentKey);

	succeed_if (value == 42, "registered value was not updated");

	keyDel (parentKey);
	keyDel (registeredKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_noUpdateWithInvalidValue ()
{
	printf ("test no update with invalid value\n");

	Key * parentKey = keyNew ("user/tests/internalnotification", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

  Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);

	int value = 123;
	succeed_if (internalnotificationRegisterInt (plugin, &value, valueKey) == 1, "call to elektraInternalnotificationRegisterInt was not successful");

	keySetString (valueKey, "42abcd");

	KeySet * ks = ksNew (1, KS_END);
	ksAppendKey (ks, valueKey);

	plugin->kdbGet (plugin, ks, parentKey);

	succeed_if (value == 123, "registered value was updated");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_updateWithValueNotYetExceedingIntMax ()
{
	printf ("test update with value = INT_MAX\n");

	Key * parentKey = keyNew ("user/tests/internalnotification", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

  Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);

	int value = 123;
	succeed_if (internalnotificationRegisterInt (plugin, &value, valueKey) == 1, "call to elektraInternalnotificationRegisterInt was not successful");

  int exceedsInt = INT_MAX;
  char * stringValue = convertLongLongToString ((long long)exceedsInt);
	keySetString (valueKey, stringValue);

	KeySet * ks = ksNew (1, KS_END);
	ksAppendKey (ks, valueKey);

	plugin->kdbGet (plugin, ks, parentKey);

	succeed_if (value == INT_MAX, "registered value was not updated");

  elektraFree (stringValue);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_noUpdateWithValueExceedingIntMax ()
{
	printf ("test no update with value that exceeds INT_MAX\n");

	Key * parentKey = keyNew ("user/tests/internalnotification", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

  Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);

	int value = 123;
	succeed_if (internalnotificationRegisterInt (plugin, &value, valueKey) == 1, "call to elektraInternalnotificationRegisterInt was not successful");

  long long exceedsInt = (long long)INT_MAX + 1;
  char * stringValue = convertLongLongToString (exceedsInt);
	keySetString (valueKey, stringValue);

	KeySet * ks = ksNew (1, KS_END);
	ksAppendKey (ks, valueKey);

	plugin->kdbGet (plugin, ks, parentKey);

	succeed_if (value == 123, "registered value was updated");

  elektraFree (stringValue);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


static void test_updateWithValueNotYetExceedingIntMin ()
{
	printf ("test update with value = INT_MIN\n");

	Key * parentKey = keyNew ("user/tests/internalnotification", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

  Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);

	int value = 123;
	succeed_if (internalnotificationRegisterInt (plugin, &value, valueKey) == 1, "call to elektraInternalnotificationRegisterInt was not successful");

  int exceedsInt = INT_MIN;
  char * stringValue = convertLongLongToString ((long long)exceedsInt);
	keySetString (valueKey, stringValue);

	KeySet * ks = ksNew (1, KS_END);
	ksAppendKey (ks, valueKey);

	plugin->kdbGet (plugin, ks, parentKey);

	succeed_if (value == INT_MIN, "registered value was not updated");

  elektraFree (stringValue);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_noUpdateWithValueExceedingIntMin ()
{
	printf ("test no update with value that exceeds INT_MIN\n");

	Key * parentKey = keyNew ("user/tests/internalnotification", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");

  Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);

	int value = 123;
	succeed_if (internalnotificationRegisterInt (plugin, &value, valueKey) == 1, "call to elektraInternalnotificationRegisterInt was not successful");

  long long exceedsInt = (long long)INT_MIN - 1;
  char * stringValue = convertLongLongToString (exceedsInt);
	keySetString (valueKey, stringValue);

	KeySet * ks = ksNew (1, KS_END);
	ksAppendKey (ks, valueKey);

	plugin->kdbGet (plugin, ks, parentKey);

	succeed_if (value == 123, "registered value was updated");

  elektraFree (stringValue);
	keyDel (parentKey);
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
	test_updateWithCascadingKey ();
  test_noUpdateWithInvalidValue ();
  test_updateWithValueNotYetExceedingIntMax ();
  test_noUpdateWithValueExceedingIntMax ();
  test_updateWithValueNotYetExceedingIntMin ();
  test_noUpdateWithValueExceedingIntMin ();

	printf ("\ntestmod_internalnotification RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
