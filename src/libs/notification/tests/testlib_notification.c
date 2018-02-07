/**
 * @file
 *
 * @brief Tests for notification libraray.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <kdbio.h>
#include <kdbnotification.h>
#include <tests.h>


void test_openclose (void)
{
	Key * key = keyNew ("/sw/tests/testlib_notification", KEY_END);
	KDB * kdb = kdbOpen (key);
	succeed_if (kdb, "opening kdb failed");

	succeed_if (!elektraNotificationClose (kdb), "could close notification system without open");

	succeed_if (elektraNotificationOpen (kdb, NULL), "could not open notification system");
	succeed_if (!elektraNotificationOpen (kdb, NULL), "could open notification system twice");

	succeed_if (elektraNotificationClose (kdb), "could not close notification system");
	succeed_if (elektraNotificationOpen (kdb, NULL), "could not re-open notification system");

	// cleanup
	succeed_if (elektraNotificationClose (kdb), "could not close notification system");
	succeed_if (kdbClose (kdb, key) == 0, "could not close kdb");
	keyDel (key);
}

void test_registerInt (void)
{
	Key * key = keyNew ("system/elektra/version/constants", KEY_END);
	Key * valueKey = keyNew ("system/elektra/version/constants/KDB_VERSION_MAJOR", KEY_END);

	int startValue = -1;
	int value = startValue;

	KDB * kdb = kdbOpen (key);

	succeed_if (elektraNotificationRegisterInt (kdb, valueKey, &value) == 0, "register should fail before open");

	elektraNotificationOpen (kdb, NULL);

	succeed_if (elektraNotificationRegisterInt (kdb, valueKey, &value), "register failed");

	// call kdbGet; value gets automatically updated
	KeySet * config = ksNew (0, KS_END);
	succeed_if (kdbGet (kdb, config, key), "kdbGet failed");

	succeed_if (value != startValue, "value was not changed");

	// cleanup
	ksDel (config);
	elektraNotificationClose (kdb);
	kdbClose (kdb, key);
	keyDel (key);
	keyDel (valueKey);
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	// Test elektraNotificationOpen and elektraNotificationClose
	test_openclose ();

	// Test elektraNotificationRegisterInt
	test_registerInt ();

	// Test elektraNotificationRegisterCallback
	// Test elektraNotificationClose

	print_result ("libnotification");

	return nbError;
}
