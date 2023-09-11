/**
 * @file
 *
 * @brief Test suite for internal data structures.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "kdbhelper.h"
#include "kdbprivate.h"
#include <tests_internal.h>

static void test_doubleGet (void)
{
	printf ("running %s\n", __func__);

	// Setup
	Key * parentKey = keyNew ("/somewhere", KS_END);
	KeySet * ks = ksNew (0, KS_END);
	KDB * kdb = kdbOpen (ksNew (0, KS_END), parentKey);
	kdbGet (kdb, ks, parentKey);
	ksAppendKey (ks, keyNew ("user:/somewhere", KEY_VALUE, "abc", KEY_END));
	ksAppendKey (ks, keyNew ("user:/somewhere/key", KEY_VALUE, "xyz", KEY_END));
	kdbSet (kdb, ks, parentKey);
	kdbClose (kdb, parentKey);

	// Scenario
	kdb = kdbOpen (ksNew (0, KS_END), parentKey);

	KeySet * ks1 = ksNew (0, KS_END);
	KeySet * ks2 = ksNew (0, KS_END);

	kdbGet (kdb, ks1, parentKey);
	succeed_if (ksLookupByName (ks1, "/somewhere/key", 0) != NULL, "should find key (1)");
	kdbGet (kdb, ks2, parentKey);
	succeed_if (ksLookupByName (ks2, "/somewhere/key", 0) != NULL, "should find key (2)");

	kdbClose (kdb, parentKey);

	ksDel (ks);
	ksDel (ks1);
	ksDel (ks2);
	keyDel (parentKey);
}

static void test_get_modified_keys (void)
{
	printf ("running %s\n", __func__);

	// Setup
	Key * parentKey = keyNew ("/somewhere", KS_END);
	KeySet * ksInitial = ksNew (0, KS_END);
	KDB * kdb = kdbOpen (ksNew (0, KS_END), parentKey);
	kdbGet (kdb, ksInitial, parentKey);
	ksAppendKey (ksInitial, keyNew ("user:/somewhere", KEY_VALUE, "abc", KEY_END));
	ksAppendKey (ksInitial, keyNew ("user:/somewhere/key", KEY_VALUE, "xyz", KEY_END));
	kdbSet (kdb, ksInitial, parentKey);
	kdbClose (kdb, parentKey);

	// Scenario
	kdb = kdbOpen (ksNew (0, KS_END), parentKey);

	KeySet * ks1 = ksNew (0, KS_END);
	KeySet * ks2 = ksNew (0, KS_END);

	kdbGet (kdb, ks1, parentKey);
	succeed_if_keyset_contains_key_with_string (ks1, "/somewhere/key", "xyz");
	ksAppendKey (ks1, keyNew ("user:/somewhere/key", KEY_VALUE, "updated", KEY_END));
	ksAppendKey (ks1, keyNew ("user:/somewhere/newkey", KEY_VALUE, "new", KEY_END));
	kdbSet (kdb, ks1, parentKey);

	kdbGet (kdb, ks2, parentKey);
	succeed_if_keyset_contains_key_with_string (ks2, "/somewhere/key", "updated");
	succeed_if_keyset_contains_key_with_string (ks2, "/somewhere/newkey", "new");

	kdbClose (kdb, parentKey);

	ksDel (ksInitial);
	ksDel (ks1);
	ksDel (ks2);
	keyDel (parentKey);
}

int main (int argc, char ** argv)
{
	printf ("INTERNAL CACHE TESTS\n");
	printf ("====================\n\n");

	init (argc, argv);

	test_doubleGet ();
	test_get_modified_keys ();

	printf ("\ntest_internalcache RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
