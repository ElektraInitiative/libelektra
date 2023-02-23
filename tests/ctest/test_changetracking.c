#include <kdb.h>
#include <kdbchangetracking.h>
#include <tests.h>

static void test_contextShouldNotBeNull (void)
{
	printf ("running %s\n", __func__);

	// Arrange
	Key * parentKey = keyNew ("user:/test", KEY_END);
	KeySet * contract = ksNew (0, KS_END);
	KDB * kdb = kdbOpen (contract, parentKey);

	// Act
	const ChangeTrackingContext * context = elektraChangeTrackingGetContextFromKdb (kdb);

	// Assert
	succeed_if (context != NULL, "context should not be null");

	kdbClose (kdb, parentKey);
	keyDel (parentKey);
	ksDel (contract);
}

static void test_shouldFindAddedKeys (void)
{
	printf ("running %s\n", __func__);

	// Arrange
	Key * parentKey = keyNew ("user:/test/add", KEY_END);
	KeySet * contract = ksNew (0, KS_END);
	KDB * kdb = kdbOpen (contract, parentKey);

	KeySet * ks = ksNew (0, KS_END);

	// Act
	kdbGet (kdb, ks, parentKey);
	ksAppendKey (ks, keyNew ("user:/test/add/123", KEY_VALUE, "xyz", KEY_END));

	const ChangeTrackingContext * context = elektraChangeTrackingGetContextFromKdb (kdb);
	ElektraDiff * diff = elektraChangeTrackingCalculateDiff (ks, context, parentKey);

	KeySet * addedKeys = elektraDiffGetAddedKeys (diff);
	KeySet * removedKeys = elektraDiffGetRemovedKeys (diff);
	KeySet * modifiedKeys = elektraDiffGetModifiedKeys (diff);

	// Assert
	succeed_if (addedKeys != NULL, "addedKeys should not be null");
	succeed_if (removedKeys != NULL, "removedKeys should not be null");
	succeed_if (modifiedKeys != NULL, "modifiedKeys should not be null");

	succeed_if (ksGetSize (addedKeys) == 1, "addedKeys should contain exactly 1 key");
	succeed_if (ksGetSize (removedKeys) == 0, "removedKeys should contain no keys");
	succeed_if (ksGetSize (modifiedKeys) == 0, "modifiedKeys should contain no keys");

	kdbClose (kdb, parentKey);
	keyDel (parentKey);
	ksDel (contract);
	ksDel (ks);

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);

	elektraDiffDel (diff);
}

static void test_shouldFindKeysWithModifiedStringValue (void)
{
	printf ("running %s\n", __func__);

	// Arrange
	Key * parentKey = keyNew ("user:/test/modify", KEY_END);
	KeySet * contract = ksNew (0, KS_END);

	KeySet * ks = ksNew (0, KS_END);

	// First store something into KDB
	KDB * kdb = kdbOpen (contract, parentKey);
	kdbGet (kdb, ks, parentKey);
	ksAppendKey (ks, keyNew ("user:/test/modify/123", KEY_VALUE, "xyz", KEY_END));
	kdbSet (kdb, ks, parentKey);
	kdbClose (kdb, parentKey);


	ksClear (ks);

	// Reopen KDB
	kdb = kdbOpen (contract, parentKey);


	// Act
	kdbGet (kdb, ks, parentKey);
	Key * key = ksLookupByName (ks, "user:/test/modify/123", 0);
	exit_if_fail (key != NULL, "did not find test key");

	keySetString (key, "some other value");

	const ChangeTrackingContext * context = elektraChangeTrackingGetContextFromKdb (kdb);
	ElektraDiff * diff = elektraChangeTrackingCalculateDiff (ks, context, parentKey);

	KeySet * addedKeys = elektraDiffGetAddedKeys (diff);
	KeySet * removedKeys = elektraDiffGetRemovedKeys (diff);
	KeySet * modifiedKeys = elektraDiffGetModifiedKeys (diff);

	// Assert
	succeed_if (addedKeys != NULL, "addedKeys should not be null");
	succeed_if (removedKeys != NULL, "removedKeys should not be null");
	succeed_if (modifiedKeys != NULL, "modifiedKeys should not be null");

	succeed_if (ksGetSize (addedKeys) == 0, "addedKeys should contain no keys");
	succeed_if (ksGetSize (removedKeys) == 0, "removedKeys should contain no keys");
	succeed_if (ksGetSize (modifiedKeys) == 1, "modifiedKeys should contain exactly 1 key");

	kdbClose (kdb, parentKey);
	keyDel (parentKey);
	ksDel (contract);
	ksDel (ks);

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);

	elektraDiffDel (diff);
}

static void test_shouldFindRemoved (void)
{
	printf ("running %s\n", __func__);

	// Arrange
	Key * parentKey = keyNew ("user:/test/remove", KEY_END);
	KeySet * contract = ksNew (0, KS_END);

	KeySet * ks = ksNew (0, KS_END);

	// First store something into KDB
	KDB * kdb = kdbOpen (contract, parentKey);
	kdbGet (kdb, ks, parentKey);
	ksAppendKey (ks, keyNew ("user:/test/remove/123", KEY_VALUE, "xyz", KEY_END));
	kdbSet (kdb, ks, parentKey);
	kdbClose (kdb, parentKey);


	ksClear (ks);

	// Reopen KDB
	kdb = kdbOpen (contract, parentKey);

	// Act
	kdbGet (kdb, ks, parentKey);
	Key * key = ksLookupByName (ks, "user:/test/remove/123", KDB_O_POP);
	exit_if_fail (key != NULL, "did not find test key");

	keySetString (key, "some other value");

	const ChangeTrackingContext * context = elektraChangeTrackingGetContextFromKdb (kdb);
	ElektraDiff * diff = elektraChangeTrackingCalculateDiff (ks, context, parentKey);

	KeySet * addedKeys = elektraDiffGetAddedKeys (diff);
	KeySet * removedKeys = elektraDiffGetRemovedKeys (diff);
	KeySet * modifiedKeys = elektraDiffGetModifiedKeys (diff);

	// Assert
	succeed_if (addedKeys != NULL, "addedKeys should not be null");
	succeed_if (removedKeys != NULL, "removedKeys should not be null");
	succeed_if (modifiedKeys != NULL, "modifiedKeys should not be null");

	succeed_if (ksGetSize (addedKeys) == 0, "addedKeys should contain no keys");
	succeed_if (ksGetSize (removedKeys) == 1, "removedKeys should contain exactly 1 key");
	succeed_if (ksGetSize (modifiedKeys) == 0, "modifiedKeys should contain no keys");

	kdbClose (kdb, parentKey);
	keyDel (parentKey);
	ksDel (contract);
	ksDel (ks);

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);

	elektraDiffDel (diff);
}

int main (int argc, char ** argv)
{
	printf ("CHANGE TRACKING       TESTS\n");
	printf ("===========================\n\n");

	init (argc, argv);

	test_contextShouldNotBeNull ();
	test_shouldFindAddedKeys ();
	test_shouldFindKeysWithModifiedStringValue ();
	test_shouldFindRemoved ();

	printf ("\ntest_backends RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
