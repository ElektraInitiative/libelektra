/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <tests_internal.h>

ssize_t ksCopyInternal (KeySet * ks, size_t to, size_t from);

static void test_elektraRenameKeys ()
{
	printf ("test rename keys\n");
	KeySet * ks = ksNew (20, keyNew ("system/some/common/prefix", KEY_END), keyNew ("system/some/common/prefix/dir", KEY_END),
			     keyNew ("system/some/common/prefix/dir/keya", KEY_END),
			     keyNew ("system/some/common/prefix/some", KEY_VALUE, "huhu", KEY_END),
			     keyNew ("system/some/common/prefix/other", KEY_END), KS_END);
	KeySet * cmp = ksNew (20, keyNew ("user/x/dir", KEY_END), keyNew ("user/x/dir/keya", KEY_END),
			      keyNew ("user/x/some", KEY_VALUE, "huhu", KEY_END), keyNew ("user/x/other", KEY_END), KS_END);

	KeySet * result = elektraRenameKeys (ks, "user/x");
	compare_keyset (result, cmp);
	// output_keyset(result);
	ksDel (cmp);
	ksDel (result);
	ksDel (ks);

	ks = ksNew (0, KS_END);
	result = elektraRenameKeys (ks, "user");
	output_keyset (result);

	ksDel (result);
	ksDel (ks);
}

static void test_elektraEmptyKeys ()
{
	printf ("test empty keys\n");
	Key * key = keyNew ("", KEY_END);
	KeySet * ks = ksNew (0, KS_END);

	elektraKeySetName (key, "", KEY_META_NAME | KEY_CASCADING_NAME);
	succeed_if_same_string (keyName (key), "");
	succeed_if (key->key != 0, "null pointer?");
	ksAppendKey (ks, key);

	succeed_if (ksLookup (ks, key, 0) == key, "could not find empty key");

	ksDel (ks);
}

static void test_cascadingLookup ()
{
	printf ("test cascading lookup\n");
	Key * k0;
	Key * k1;
	Key * k2;
	Key * k3;
	KeySet * ks = ksNew (10, k0 = keyNew ("system/benchmark/override/#0", 0), k1 = keyNew ("system/benchmark/override/#1", 0),
			     k2 = keyNew ("user/benchmark/override/#2", 0), k3 = keyNew ("user/benchmark/override/#3", 0), KS_END);
	Key * search = keyNew ("/benchmark/override/#0", KEY_CASCADING_NAME, KEY_END);
	Key * found = ksLookup (ks, search, 0);
	succeed_if (found == k0, "found wrong key");

	elektraKeySetName (search, "/benchmark/override/#1", KEY_CASCADING_NAME);
	found = ksLookup (ks, search, 0);
	succeed_if (found == k1, "found wrong key");
	keyDel (search);

	search = keyNew ("/benchmark/override/#2", KEY_CASCADING_NAME, KEY_END);
	found = ksLookup (ks, search, 0);
	succeed_if (found == k2, "found wrong key");

	elektraKeySetName (search, "/benchmark/override/#3", KEY_CASCADING_NAME);
	found = ksLookup (ks, search, 0);
	succeed_if (found == k3, "found wrong key");
	keyDel (search);
	ksDel (ks);
}

static void test_creatingLookup ()
{
	printf ("Test creating lookup\n");

	KeySet * ks = ksNew (10, KS_END);

	Key * searchKey = keyNew ("user/something", KEY_VALUE, "a value", KEY_END);
	Key * k0 = ksLookup (ks, searchKey, KDB_O_CREATE);
	exit_if_fail (k0, "no key was created");
	succeed_if_same_string (keyName (k0), keyName (searchKey));
	succeed_if_same_string (keyString (k0), keyString (searchKey));

	Key * k1 = ksLookup (ks, searchKey, KDB_O_CREATE);
	exit_if_fail (k1, "no key was returned");
	succeed_if (k0 == k1, "not the same key");

	keyDel (searchKey);
	ksDel (ks);


	ks = ksNew (10, KS_END);

	searchKey = keyNew ("dir/something", KEY_VALUE, "a value", KEY_END);
	k0 = ksLookup (ks, searchKey, KDB_O_CREATE);
	exit_if_fail (k0, "no key was created");
	succeed_if_same_string (keyName (k0), keyName (searchKey));
	succeed_if_same_string (keyString (k0), keyString (searchKey));

	k1 = ksLookup (ks, searchKey, KDB_O_CREATE);
	exit_if_fail (k1, "no key was returned");
	succeed_if (k0 == k1, "not the same key");

	keyDel (searchKey);
	ksDel (ks);


	ks = ksNew (10, KS_END);

	searchKey = keyNew ("/something", KEY_CASCADING_NAME, KEY_VALUE, "a value", KEY_END);

	// check if duplication works:
	Key * dupKey = keyDup (searchKey);
	succeed_if_same_string (keyName (dupKey), keyName (searchKey));
	succeed_if_same_string (keyString (dupKey), keyString (searchKey));
	ksAppendKey (ks, dupKey);
	keyDel (dupKey);

	k0 = ksLookup (ks, searchKey, KDB_O_CREATE);
	exit_if_fail (k0, "no key was created");
	succeed_if_same_string (keyName (k0), keyName (searchKey));
	succeed_if_same_string (keyString (k0), keyString (searchKey));

	k1 = ksLookup (ks, searchKey, KDB_O_CREATE);
	exit_if_fail (k1, "no key was returned");
	succeed_if (k0 == k1, "not the same key");

	keyDel (searchKey);
	ksDel (ks);


	ks = ksNew (10, KS_END);

	searchKey = keyNew ("proc/something", KEY_VALUE, "a value", KEY_END);
	k0 = ksLookup (ks, searchKey, KDB_O_CREATE);
	exit_if_fail (k0, "no key was created");
	succeed_if_same_string (keyName (k0), keyName (searchKey));
	succeed_if_same_string (keyString (k0), keyString (searchKey));

	k1 = ksLookup (ks, searchKey, KDB_O_CREATE);
	exit_if_fail (k1, "no key was returned");
	succeed_if (k0 == k1, "not the same key");

	keyDel (searchKey);
	ksDel (ks);
}

int main (int argc, char ** argv)
{
	printf ("KS         TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_elektraRenameKeys ();
	test_elektraEmptyKeys ();
	test_cascadingLookup ();
	test_creatingLookup ();

	printf ("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
