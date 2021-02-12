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
	Key * search = keyNew ("/benchmark/override/#0", KEY_END);
	Key * found = ksLookup (ks, search, 0);
	succeed_if (found == k0, "found wrong key");

	keySetName (search, "/benchmark/override/#1");
	found = ksLookup (ks, search, 0);
	succeed_if (found == k1, "found wrong key");
	keyDel (search);

	search = keyNew ("/benchmark/override/#2", KEY_END);
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

	searchKey = keyNew ("/something", KEY_VALUE, "a value", KEY_END);

	// check if duplication works:
	Key * dupKey = keyDup (searchKey, KEY_CP_ALL);
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
	printf ("Test ksToArray\n");

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
	printf ("Test no alloc\n");

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

static void test_ksRename (void)
{
	printf ("Test ksRename\n");

	Key * key1 = keyNew ("system:/baz", KEY_END);
	Key * key2 = keyNew ("system:/baz/bar", KEY_END);
	Key * key3 = keyNew ("system:/baz/bar/bar", KEY_END);
	Key * key4 = keyNew ("system:/baz/bar/foo", KEY_END);

	KeySet * ks = ksNew (24,
			     // clang-format off
			     keyNew ("system:/bar", KEY_END), 
			     keyNew ("system:/bar/bar", KEY_END),
			     keyNew ("system:/bar/bar/bar", KEY_END),
			     keyNew ("system:/bar/bar/foo", KEY_END),
			     key1,
			     key2,
			     key3,
			     key4,
			     keyNew ("system:/foo", KEY_END), 
			     keyNew ("system:/foo/bar", KEY_END),
			     keyNew ("system:/foo/bar/bar", KEY_END),
			     keyNew ("system:/foo/bar/foo", KEY_END),
			     // clang-format on
			     KS_END);

	Key * renamed1 = keyNew ("dir:/baz", KEY_END);
	Key * renamed2 = keyNew ("dir:/baz/bar", KEY_END);
	Key * renamed3 = keyNew ("dir:/baz/bar/bar", KEY_END);
	Key * renamed4 = keyNew ("dir:/baz/bar/foo", KEY_END);

	KeySet * renamed = ksNew (
		24,
		// clang-format off
		renamed1, 
		renamed2,
		renamed3,
		renamed4,
		keyNew ("system:/bar", KEY_END), 
		keyNew ("system:/bar/bar", KEY_END),
		keyNew ("system:/bar/bar/bar", KEY_END),
		keyNew ("system:/bar/bar/foo", KEY_END),
		keyNew ("system:/foo", KEY_END), 
		keyNew ("system:/foo/bar", KEY_END),
		keyNew ("system:/foo/bar/bar", KEY_END),
		keyNew ("system:/foo/bar/foo", KEY_END),
		// clang-format on
		KS_END);

	Key * orig1 = keyNew ("system:/baz", KEY_END);
	Key * orig2 = keyNew ("system:/baz/bar", KEY_END);
	Key * orig3 = keyNew ("system:/baz/bar/bar", KEY_END);
	Key * orig4 = keyNew ("system:/baz/bar/foo", KEY_END);

	KeySet * orig = ksDeepDup (ks);

	Key * root = keyNew ("user:/baz", KEY_END);
	Key * newRoot = keyNew ("user:/baz", KEY_END);

	succeed_if (ksRename (NULL, root, newRoot) == -1, "shouldn't accept NULL pointers");
	succeed_if (ksRename (ks, NULL, newRoot) == -1, "shouldn't accept NULL pointers");
	succeed_if (ksRename (ks, root, NULL) == -1, "shouldn't accept NULL pointers");

	keySetName (root, "/baz");
	succeed_if (ksRename (ks, root, newRoot) == -1, "shouldn't accept cascading keys");
	compare_keyset (ks, orig);

	keySetName (root, "user:/baz");
	keySetName (newRoot, "/baz");
	succeed_if (ksRename (ks, root, newRoot) == -1, "shouldn't accept cascading keys");
	compare_keyset (ks, orig);

	keySetName (root, "system:/zzzz");
	keySetName (newRoot, "dir:/baz");
	succeed_if (ksRename (ks, root, newRoot) == 0, "root not found should be nop");
	compare_keyset (ks, orig);

	keySetName (root, "system:/zzzz");
	keySetName (newRoot, "system:/baz");
	succeed_if (ksRename (ks, root, newRoot) == 0, "root not found should be nop");
	compare_keyset (ks, orig);

	keySetName (root, "system:/baz");
	keySetName (newRoot, "system:/baz");
	succeed_if (ksRename (ks, root, newRoot) == 4, "same root should always work");
	compare_keyset (ks, orig);

	keySetName (root, "system:/bar");
	keySetName (newRoot, "system:/baz");
	succeed_if (ksRename (ks, root, newRoot) == -2, "should fail with existing keys below newRoot");
	compare_keyset (ks, orig);

	keySetName (root, "system:/baz");
	keySetName (newRoot, "dir:/baz");
	// keys only referenced by ks
	succeed_if (ksRename (ks, root, newRoot) == 4, "didn't rename correctly");
	compare_keyset (ks, renamed);
	succeed_if (keyCmp (key1, renamed1) == 0, "should have renamed in-place");
	succeed_if (keyCmp (key2, renamed2) == 0, "should have renamed in-place");
	succeed_if (keyCmp (key3, renamed3) == 0, "should have renamed in-place");
	succeed_if (keyCmp (key4, renamed4) == 0, "should have renamed in-place");

	ksDel (ks);
	ks = ksDup (orig);
	key1 = ksLookup (ks, orig1, 0);
	key2 = ksLookup (ks, orig2, 0);
	key3 = ksLookup (ks, orig3, 0);
	key4 = ksLookup (ks, orig4, 0);

	keySetName (root, "system:/baz");
	keySetName (newRoot, "dir:/baz");
	// keys referenced by ks and orig
	succeed_if (ksRename (ks, root, newRoot) == 4, "didn't rename correctly");
	compare_keyset (ks, renamed);
	succeed_if (keyCmp (key1, orig1) == 0, "should have dup'ed key");
	succeed_if (keyCmp (key2, orig2) == 0, "should have dup'ed key");
	succeed_if (keyCmp (key3, orig3) == 0, "should have dup'ed key");
	succeed_if (keyCmp (key4, orig4) == 0, "should have dup'ed key");

	ksDel (ks);
	ksDel (orig);
	ksDel (renamed);
	keyDel (orig1);
	keyDel (orig2);
	keyDel (orig3);
	keyDel (orig4);
	keyDel (root);
	keyDel (newRoot);
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
	test_ksRename ();

	printf ("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
