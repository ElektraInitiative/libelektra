/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbease.h>
#include <tests_internal.h>

ssize_t ksCopyInternal (KeySet * ks, size_t to, size_t from);

static void test_ksRenameKeys (void)
{
	printf ("test rename keys\n");
	KeySet * ks = ksNew (20, keyNew ("system:/some/common/prefix", KEY_END), keyNew ("system:/some/common/prefix/dir", KEY_END),
			     keyNew ("system:/some/common/prefix/dir/keya", KEY_END),
			     keyNew ("system:/some/common/prefix/some", KEY_VALUE, "huhu", KEY_END),
			     keyNew ("system:/some/common/prefix/other", KEY_END), KS_END);
	KeySet * cmp = ksNew (20, keyNew ("user:/x/dir", KEY_END), keyNew ("user:/x/dir/keya", KEY_END),
			      keyNew ("user:/x/some", KEY_VALUE, "huhu", KEY_END), keyNew ("user:/x/other", KEY_END), KS_END);

	KeySet * result = ksRenameKeys (ks, "user:/x");
	compare_keyset (result, cmp);
	// output_keyset(result);
	ksDel (cmp);
	ksDel (result);
	ksDel (ks);

	ks = ksNew (0, KS_END);
	result = ksRenameKeys (ks, "user");
	output_keyset (result);

	ksDel (result);
	ksDel (ks);
}

static void test_cascadingLookup (void)
{
	printf ("test cascading lookup\n");
	Key * k0;
	Key * k1;
	Key * k2;
	Key * k3;
	KeySet * ks = ksNew (10, k0 = keyNew ("system:/benchmark/override/#0", 0), k1 = keyNew ("system:/benchmark/override/#1", 0),
			     k2 = keyNew ("user:/benchmark/override/#2", 0), k3 = keyNew ("user:/benchmark/override/#3", 0), KS_END);
	Key * search = keyNew ("/benchmark/override/#0", KEY_CASCADING_NAME, KEY_END);
	Key * found = ksLookup (ks, search, 0);
	succeed_if (found == k0, "found wrong key");

	keySetName (search, "/benchmark/override/#1");
	found = ksLookup (ks, search, 0);
	succeed_if (found == k1, "found wrong key");
	keyDel (search);

	search = keyNew ("/benchmark/override/#2", KEY_CASCADING_NAME, KEY_END);
	found = ksLookup (ks, search, 0);
	succeed_if (found == k2, "found wrong key");

	keySetName (search, "/benchmark/override/#3");
	found = ksLookup (ks, search, 0);
	succeed_if (found == k3, "found wrong key");
	keyDel (search);
	ksDel (ks);
}

static void test_creatingLookup (void)
{
	printf ("Test creating lookup\n");

	KeySet * ks = ksNew (10, KS_END);

	Key * searchKey = keyNew ("user:/something", KEY_VALUE, "a value", KEY_END);
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

	searchKey = keyNew ("dir:/something", KEY_VALUE, "a value", KEY_END);
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

	searchKey = keyNew ("proc:/something", KEY_VALUE, "a value", KEY_END);
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


static void test_ksToArray (void)
{
	KeySet * ks = ksNew (5, keyNew ("user:/test1", KEY_END), keyNew ("user:/test2", KEY_END), keyNew ("user:/test3", KEY_END), KS_END);

	Key ** keyArray = calloc (ksGetSize (ks), sizeof (Key *));
	elektraKsToMemArray (ks, keyArray);

	succeed_if_same_string ("user:/test1", keyName (keyArray[0]));
	succeed_if_same_string ("user:/test2", keyName (keyArray[1]));
	succeed_if_same_string ("user:/test3", keyName (keyArray[2]));

	/* test if cursor is restored */
	ksNext (ks);
	elektraCursor cursor = ksGetCursor (ks);
	elektraKsToMemArray (ks, keyArray);

	succeed_if (ksGetCursor (ks) == cursor, "cursor was not restored");

	succeed_if (elektraKsToMemArray (0, keyArray) < 0, "wrong result on null pointer");
	succeed_if (elektraKsToMemArray (ks, 0) < 0, "wrong result on null buffer");
	KeySet * empty = ksNew (0, KS_END);
	succeed_if (elektraKsToMemArray (empty, keyArray) == 0, "wrong result on empty keyset");
	ksDel (empty);

	elektraFree (keyArray);
	ksDel (ks);
}

static void test_ksNoAlloc (void)
{
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (ks->alloc == 0, "alloc is not 0");
	succeed_if (ks->size == 0, "size is not 0");
	succeed_if (ks->array == NULL, "array is not NULL");

	ksDel (ks);

	ks = ksNew (1, KS_END);

	succeed_if (ks->alloc != 0, "alloc is 0");
	succeed_if (ks->size == 0, "size is not 0");
	succeed_if (ks->array != NULL, "array is NULL");

	ksDel (ks);
}

int main (int argc, char ** argv)
{
	printf ("KS         TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_ksToArray ();
	test_ksRenameKeys ();
	test_cascadingLookup ();
	test_creatingLookup ();
	test_ksNoAlloc ();

	printf ("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
