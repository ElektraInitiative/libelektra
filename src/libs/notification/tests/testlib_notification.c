/**
 * @file
 *
 * @brief Tests for notification library.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <kdbio.h>
#include <kdbnotification.h>
#include <tests.h>

int callback_called;

static void test_registerInt (void)
{
	printf ("test elektraNotificationRegisterInt\n");

	ElektraKey * key = keyNew ("system:/elektra/version/constants", KEY_END);
	ElektraKey * valueKey = keyNew ("system:/elektra/version/constants/KDB_VERSION_MAJOR", KEY_END);

	int startValue = -1;
	int value = startValue;

	ElektraKdb * kdb = kdbOpen (NULL, key);

	succeed_if (elektraNotificationRegisterInt (kdb, valueKey, &value) == 0, "register should fail without contract");

	kdbClose (kdb, key);

	ElektraKeyset * contract = ksNew (0, KS_END);
	elektraNotificationContract (contract);
	kdb = kdbOpen (contract, key);

	succeed_if (elektraNotificationRegisterInt (kdb, valueKey, &value), "register failed");

	// call kdbGet; value gets automatically updated
	ElektraKeyset * config = ksNew (0, KS_END);
	succeed_if (kdbGet (kdb, config, key), "kdbGet failed");

	succeed_if (value != startValue, "value was not changed");

	// cleanup
	ksDel (config);
	ksDel (contract);
	kdbClose (kdb, key);
	keyDel (key);
	keyDel (valueKey);
}

static void testCallback (ElektraKey * key ELEKTRA_UNUSED, void * context ELEKTRA_UNUSED)
{
	callback_called = 1;
}

static void test_registerCallback (void)
{
	printf ("test elektraNotificationRegisterCallback\n");

	ElektraKey * key = keyNew ("system:/elektra/version/constants", KEY_END);
	ElektraKey * valueKey = keyNew ("system:/elektra/version/constants/KDB_VERSION_MAJOR", KEY_END);
	callback_called = 0;

	ElektraKdb * kdb = kdbOpen (NULL, key);

	succeed_if (elektraNotificationRegisterCallback (kdb, valueKey, testCallback, NULL) == 0, "register should fail without contract");

	kdbClose (kdb, key);

	ElektraKeyset * contract = ksNew (0, KS_END);
	elektraNotificationContract (contract);
	kdb = kdbOpen (contract, key);

	succeed_if (elektraNotificationRegisterCallback (kdb, valueKey, testCallback, NULL), "register failed");

	// call kdbGet; value gets automatically updated
	ElektraKeyset * config = ksNew (0, KS_END);
	succeed_if (kdbGet (kdb, config, key), "kdbGet failed");

	succeed_if (callback_called, "callback was not called");

	// cleanup
	ksDel (config);
	ksDel (contract);
	kdbClose (kdb, key);
	keyDel (key);
	keyDel (valueKey);
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	// Test elektraNotificationRegisterInt
	test_registerInt ();

	// Test elektraNotificationRegisterCallback
	test_registerCallback ();

	print_result ("libnotification");

	return nbError;
}
