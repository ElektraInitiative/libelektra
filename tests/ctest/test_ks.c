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
	KeySet * ks =
		ksNew (10, k0 = keyNew ("system:/benchmark/override/#0", KEY_END), k1 = keyNew ("system:/benchmark/override/#1", KEY_END),
		       k2 = keyNew ("user:/benchmark/override/#2", KEY_END), k3 = keyNew ("user:/benchmark/override/#3", KEY_END), KS_END);
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

static void test_circularLinkLookup (void)
{
	printf ("Test circular link lookup\n");
	KeySet * ks = ksNew (10, keyNew ("spec:/circular", KEY_META, "override/#0", "/circular", KEY_END), KS_END);
	Key * search = keyNew ("/circular", KEY_END);
	Key * found = ksLookup (ks, search, 0);
	succeed_if (found == NULL, "found nonexistent key");
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

	succeed_if (ks->data == NULL, "should not allocate data");

	ksDel (ks);

	ks = ksNew (1, KS_END);

	succeed_if (ks->data->alloc != 0, "alloc is 0");
	succeed_if (ks->data->size == 0, "size is not 0");
	succeed_if (ks->data->array != NULL, "array is NULL");

	ksDel (ks);
}

static void test_ksRename (void)
{
	printf ("Test ksRename\n");

	Key * key1 = keyNew ("system:/baz", KEY_VALUE, "5", KEY_END);
	Key * key2 = keyNew ("system:/baz/bar", KEY_VALUE, "6", KEY_END);
	Key * key3 = keyNew ("system:/baz/bar/bar", KEY_VALUE, "7", KEY_END);
	Key * key4 = keyNew ("system:/baz/bar/foo", KEY_VALUE, "8", KEY_END);

	KeySet * ks = ksNew (
		24,
		// clang-format off
		keyNew ("system:/bar", KEY_VALUE, "1", KEY_END), 
		keyNew ("system:/bar/bar", KEY_VALUE, "2", KEY_END),
		keyNew ("system:/bar/bar/bar", KEY_VALUE, "3", KEY_END),
		keyNew ("system:/bar/bar/foo", KEY_VALUE, "4", KEY_END),
		key1,
		key2,
		key3,
		key4,
		keyNew ("system:/foo", KEY_VALUE, "9", KEY_END), 
		keyNew ("system:/foo/bar", KEY_VALUE, "10", KEY_END),
		keyNew ("system:/foo/bar/bar", KEY_VALUE, "11", KEY_END),
		keyNew ("system:/foo/bar/foo", KEY_VALUE, "12", KEY_END),
		// clang-format on
		KS_END);

	Key * keyRenamed1 = keyNew ("dir:/baz", KEY_VALUE, "5", KEY_END);
	Key * keyRenamed2 = keyNew ("dir:/baz/bar", KEY_VALUE, "6", KEY_END);
	Key * keyRenamed3 = keyNew ("dir:/baz/bar/bar", KEY_VALUE, "7", KEY_END);
	Key * keyRenamed4 = keyNew ("dir:/baz/bar/foo", KEY_VALUE, "8", KEY_END);

	KeySet * renamed =
		ksNew (24,
		       // clang-format off
		       keyRenamed1, 
		       keyRenamed2,
		       keyRenamed3,
		       keyRenamed4,
		       keyNew ("system:/bar", KEY_VALUE, "1", KEY_END), 
		       keyNew ("system:/bar/bar", KEY_VALUE, "2", KEY_END),
		       keyNew ("system:/bar/bar/bar", KEY_VALUE, "3", KEY_END),
		       keyNew ("system:/bar/bar/foo", KEY_VALUE, "4", KEY_END),
		       keyNew ("system:/foo", KEY_VALUE, "9", KEY_END), 
		       keyNew ("system:/foo/bar", KEY_VALUE, "10", KEY_END),
		       keyNew ("system:/foo/bar/bar", KEY_VALUE, "11", KEY_END),
		       keyNew ("system:/foo/bar/foo", KEY_VALUE, "12", KEY_END),
		       // clang-format on
		       KS_END);

	Key * orig1 = keyNew ("system:/baz", KEY_VALUE, "5", KEY_END);
	Key * orig2 = keyNew ("system:/baz/bar", KEY_VALUE, "6", KEY_END);
	Key * orig3 = keyNew ("system:/baz/bar/bar", KEY_VALUE, "7", KEY_END);
	Key * orig4 = keyNew ("system:/baz/bar/foo", KEY_VALUE, "8", KEY_END);

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

	keySetName (root, "system:/baz");
	keySetName (newRoot, "dir:/baz");
	// keys only referenced by ks
	succeed_if (ksRename (ks, root, newRoot) == 4, "didn't rename correctly");
	compare_keyset (ks, renamed);
	succeed_if (keyCmp (key1, keyRenamed1) == 0, "should have renamed in-place");
	succeed_if (keyCmp (key2, keyRenamed2) == 0, "should have renamed in-place");
	succeed_if (keyCmp (key3, keyRenamed3) == 0, "should have renamed in-place");
	succeed_if (keyCmp (key4, keyRenamed4) == 0, "should have renamed in-place");

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

	ksDel (renamed);
	renamed = ksNew (24,
			 // clang-format off
			 keyNew ("system:/bar", KEY_VALUE, "1", KEY_END), 
			 keyNew ("system:/bar/bar", KEY_VALUE, "2", KEY_END),
			 keyNew ("system:/bar/bar/bar", KEY_VALUE, "3", KEY_END),
			 keyNew ("system:/bar/bar/foo", KEY_VALUE, "4", KEY_END),
			 keyNew ("system:/baz/baz", KEY_VALUE, "5", KEY_END),
			 keyNew ("system:/baz/baz/bar", KEY_VALUE, "6", KEY_END),
			 keyNew ("system:/baz/baz/bar/bar", KEY_VALUE, "7", KEY_END),
			 keyNew ("system:/baz/baz/bar/foo", KEY_VALUE, "8", KEY_END),
			 keyNew ("system:/foo", KEY_VALUE, "9", KEY_END), 
			 keyNew ("system:/foo/bar", KEY_VALUE, "10", KEY_END),
			 keyNew ("system:/foo/bar/bar", KEY_VALUE, "11", KEY_END),
			 keyNew ("system:/foo/bar/foo", KEY_VALUE, "12", KEY_END),
			 // clang-format on
			 KS_END);

	ksDel (ks);
	ks = ksDup (orig);
	keySetName (root, "system:/baz");
	keySetName (newRoot, "system:/baz/baz");
	succeed_if (ksRename (ks, root, newRoot) == 4, "didn't rename correctly");
	compare_keyset (ks, renamed);

	ksDel (renamed);
	renamed = ksNew (24,
			 // clang-format off
			 keyNew ("system:/bar", KEY_VALUE, "5", KEY_END), 
			 keyNew ("system:/bar/bar", KEY_VALUE, "6", KEY_END),
			 keyNew ("system:/bar/bar/bar", KEY_VALUE, "7", KEY_END),
			 keyNew ("system:/bar/bar/foo", KEY_VALUE, "8", KEY_END),
			 keyNew ("system:/foo", KEY_VALUE, "9", KEY_END), 
			 keyNew ("system:/foo/bar", KEY_VALUE, "10", KEY_END),
			 keyNew ("system:/foo/bar/bar", KEY_VALUE, "11", KEY_END),
			 keyNew ("system:/foo/bar/foo", KEY_VALUE, "12", KEY_END),
			 // clang-format on
			 KS_END);

	ksDel (ks);
	ks = ksDup (orig);
	keySetName (root, "system:/baz");
	keySetName (newRoot, "system:/bar");
	succeed_if (ksRename (ks, root, newRoot) == 4, "should work even with existing keys below newRoot");
	compare_keyset (ks, renamed);

	ksDel (renamed);
	renamed = ksNew (24,
			 // clang-format off
			 keyNew ("system:/", KEY_VALUE, "1", KEY_END), 
			 keyNew ("system:/bar", KEY_VALUE, "2", KEY_END),
			 keyNew ("system:/bar/bar", KEY_VALUE, "3", KEY_END),
			 keyNew ("system:/bar/foo", KEY_VALUE, "4", KEY_END),
			 keyNew ("system:/baz", KEY_VALUE, "5", KEY_END),
			 keyNew ("system:/baz/bar", KEY_VALUE, "6", KEY_END),
			 keyNew ("system:/baz/bar/bar", KEY_VALUE, "7", KEY_END),
			 keyNew ("system:/baz/bar/foo", KEY_VALUE, "8", KEY_END),
			 keyNew ("system:/foo", KEY_VALUE, "9", KEY_END), 
			 keyNew ("system:/foo/bar", KEY_VALUE, "10", KEY_END),
			 keyNew ("system:/foo/bar/bar", KEY_VALUE, "11", KEY_END),
			 keyNew ("system:/foo/bar/foo", KEY_VALUE, "12", KEY_END),
			 // clang-format on
			 KS_END);

	ksDel (ks);
	ks = ksDup (orig);
	keySetName (root, "system:/bar");
	keySetName (newRoot, "system:/");
	succeed_if (ksRename (ks, root, newRoot) == 4, "didn't rename correctly");
	compare_keyset (ks, renamed);

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

void test_ksFindHierarchy (void)
{
	printf ("Test ksFindHierarchy\n");

	KeySet * ks =
		ksNew (24,
		       // clang-format off
		       keyNew ("system:/bar", KEY_VALUE, "1", KEY_END), 
		       keyNew ("system:/bar/bar", KEY_VALUE, "2", KEY_END),
		       keyNew ("system:/bar/bar/bar", KEY_VALUE, "3", KEY_END),
		       keyNew ("system:/bar/bar/foo", KEY_VALUE, "4", KEY_END),
		       keyNew ("system:/baz", KEY_VALUE, "5", KEY_END),
		       keyNew ("system:/baz/bar", KEY_VALUE, "6", KEY_END),
		       keyNew ("system:/baz/bar/bar", KEY_VALUE, "7", KEY_END),
		       keyNew ("system:/baz/bar/foo", KEY_VALUE, "8", KEY_END),
		       keyNew ("system:/foo", KEY_VALUE, "9", KEY_END), 
		       keyNew ("system:/foo/bar", KEY_VALUE, "10", KEY_END),
		       keyNew ("system:/foo/bar/bar", KEY_VALUE, "11", KEY_END),
		       keyNew ("system:/foo/bar/foo", KEY_VALUE, "12", KEY_END),
		       // clang-format on
		       KS_END);

	Key * root = keyNew ("/", KEY_END);

	elektraCursor end;

	succeed_if (ksFindHierarchy (ks, NULL, &end) == -1, "shouldn't accept NULL");
	succeed_if (ksFindHierarchy (NULL, root, &end) == -1, "shouldn't accept NULL");

	succeed_if (ksFindHierarchy (ks, root, &end) == ksGetSize (ks), "shouldn't find key");

	keySetName (root, "user:/");
	succeed_if (ksFindHierarchy (ks, root, &end) == ksGetSize (ks), "shouldn't find key");

	keySetName (root, "system:/zoo");
	succeed_if (ksFindHierarchy (ks, root, &end) == ksGetSize (ks), "shouldn't find key");

	keySetName (root, "system:/bar/foo");
	succeed_if (ksFindHierarchy (ks, root, &end) == ksGetSize (ks), "shouldn't find key");

	keySetName (root, "system:/");
	succeed_if (ksFindHierarchy (ks, root, &end) == 0 && end == 12, "hierarchy should be present");

	keySetName (root, "system:/bar");
	succeed_if (ksFindHierarchy (ks, root, &end) == 0 && end == 4, "hierarchy should be present");

	keySetName (root, "system:/bar/bar");
	succeed_if (ksFindHierarchy (ks, root, &end) == 1 && end == 4, "hierarchy should be present");

	keySetName (root, "system:/baz/bar/bar");
	succeed_if (ksFindHierarchy (ks, root, &end) == 6 && end == 7, "hierarchy should be present");

	keySetName (root, "system:/baz/bar/bar");
	succeed_if (ksFindHierarchy (ks, root, NULL) == 6, "should accept NULL for end");

	keyDel (root);
	ksDel (ks);
}

static KeySet * set_a (void)
{
	return ksNew (16, keyNew ("user:/0", KEY_END), keyNew ("user:/a", KEY_END), keyNew ("user:/a/a", KEY_END),
		      keyNew ("user:/a/a/a", KEY_END), keyNew ("user:/a/a/b", KEY_END), keyNew ("user:/a/b", KEY_END),
		      keyNew ("user:/a/b/a", KEY_END), keyNew ("user:/a/b/b", KEY_END), keyNew ("user:/a/c", KEY_END),
		      keyNew ("user:/a/d", KEY_END), keyNew ("user:/a/x/a", KEY_END), keyNew ("user:/a/x/b", KEY_END),
		      keyNew ("user:/a/x/c", KEY_END), keyNew ("user:/a/x/c/a", KEY_END), keyNew ("user:/a/x/c/b", KEY_END),
		      keyNew ("user:/x", KEY_END), KS_END);
}

static void test_ksSearch (void)
{
	printf ("Testing ksSearch\n");

	KeySet * a = set_a ();
	Key * s = keyNew ("user:/a", KEY_END);
	ssize_t result;

	keySetName (s, "user:/0");
	result = ksSearch (a, s);
	succeed_if (result == 0, "insertpos wrong");

	keySetName (s, "user:/a");
	result = ksSearch (a, s);
	succeed_if (result == 1, "insertpos wrong");

	keySetName (s, "user:/a/0");
	result = ksSearch (a, s);
	succeed_if (result == -3, "insertpos wrong");

	keySetName (s, "user:/a/a");
	result = ksSearch (a, s);
	succeed_if (result == 2, "insertpos wrong");

	keySetName (s, "user:/a/a/a");
	result = ksSearch (a, s);
	succeed_if (result == 3, "insertpos wrong");

	keySetName (s, "user:/a/a/b");
	result = ksSearch (a, s);
	succeed_if (result == 4, "insertpos wrong");

	keySetName (s, "user:/a/b");
	result = ksSearch (a, s);
	succeed_if (result == 5, "insertpos wrong");

	keySetName (s, "user:/a/b/a");
	result = ksSearch (a, s);
	succeed_if (result == 6, "insertpos wrong");

	keySetName (s, "user:/a/b/b");
	result = ksSearch (a, s);
	succeed_if (result == 7, "insertpos wrong");

	keySetName (s, "user:/a/c");
	result = ksSearch (a, s);
	succeed_if (result == 8, "insertpos wrong");

	keySetName (s, "user:/a/d");
	result = ksSearch (a, s);
	succeed_if (result == 9, "insertpos wrong");

	keySetName (s, "user:/a/x");
	result = ksSearch (a, s);
	succeed_if (result == -11, "insertpos wrong");

	keySetName (s, "user:/a/x/a");
	result = ksSearch (a, s);
	succeed_if (result == 10, "insertpos wrong");

	keySetName (s, "user:/a/x/b");
	result = ksSearch (a, s);
	succeed_if (result == 11, "insertpos wrong");

	keySetName (s, "user:/a/x/c");
	result = ksSearch (a, s);
	succeed_if (result == 12, "insertpos wrong");

	keySetName (s, "user:/a/x/c/a");
	result = ksSearch (a, s);
	succeed_if (result == 13, "insertpos wrong");

	keySetName (s, "user:/a/x/c/b");
	result = ksSearch (a, s);
	succeed_if (result == 14, "insertpos wrong");

	keySetName (s, "user:/x");
	result = ksSearch (a, s);
	succeed_if (result == 15, "insertpos wrong");

	/*
	   Generation of new test cases:
	for (int i=0; i< 16; ++i)
	{
		s = a->array[i];
		printf ("keySetName (s, \"%s\");\n", keyName(s));
		printf ("result = ksSearch (a, s);\n");
		printf ("succeed_if (result == %zd, \"insertpos wrong\");\n\n", ksSearch (a, s));
	}
	*/

	keyDel (s);
	ksDel (a);
}

static void test_ksSubtract_nullParameters (void)
{
	printf ("Testing ksSubtract (NULL as parameters)\n");

	KeySet * total = ksNew (0, KS_END);
	KeySet * sub = ksNew (0, KS_END);

	ssize_t result = ksSubtract (NULL, NULL);
	succeed_if (result == -1, "should return -1 when both are NULL");

	result = ksSubtract (NULL, sub);
	succeed_if (result == -1, "should return -1 when total is NULL");

	result = ksSubtract (total, NULL);
	succeed_if (result == -1, "should return -1 when sub is NULL");

	ksDel (total);
	ksDel (sub);
}

static void test_ksSubtract_emptyParameters (void)
{
	printf ("Testing ksSubtract (empty parameters)\n");

	KeySet * total = ksNew (0, KS_END);
	KeySet * sub = ksNew (0, KS_END);
	KeySet * nonEmpty = ksNew (1, keyNew ("user:/test", KEY_END), KS_END);

	ssize_t result = ksSubtract (total, sub);
	succeed_if (result == 0, "should return 0 when both are empty");

	result = ksSubtract (nonEmpty, sub);
	succeed_if (result == 0, "should return 0 when total is empty");

	result = ksSubtract (total, nonEmpty);
	succeed_if (result == 0, "should return 0 when sub is empty");

	ksDel (total);
	ksDel (sub);
	ksDel (nonEmpty);
}

static void test_ksSubtract_1 (void)
{
	printf ("Testing ksSubtract (1)\n");

	// Arrange
	KeySet * total = ksNew (10, keyNew ("user:/test/k1", KEY_END), keyNew ("user:/test/k2", KEY_END), keyNew ("user:/test/k3", KEY_END),
				keyNew ("user:/test/k4", KEY_END), keyNew ("user:/test/k5", KEY_END), KS_END);

	KeySet * sub = ksNew (10, keyNew ("user:/test/k1", KEY_END), keyNew ("user:/test/k2", KEY_END), keyNew ("user:/test/k3", KEY_END),
			      keyNew ("user:/test/k4", KEY_END), keyNew ("user:/test/k5", KEY_END), KS_END);

	// Act
	ssize_t result = ksSubtract (total, sub);

	// Assert
	succeed_if_fmt (result == 5, "should have removed 5 keys (was %zu)", result);
	succeed_if (ksGetSize (total) == 0, "total should be empty");

	ksDel (total);
	ksDel (sub);
}

static void test_ksSubtract_2 (void)
{
	printf ("Testing ksSubtract (2)\n");

	// Arrange
	KeySet * total = ksNew (10, keyNew ("user:/test/k2", KEY_END), keyNew ("user:/test/k3", KEY_END), KS_END);

	KeySet * sub = ksNew (10, keyNew ("user:/test/k1", KEY_END), keyNew ("user:/test/k2", KEY_END), keyNew ("user:/test/k3", KEY_END),
			      keyNew ("user:/test/k4", KEY_END), keyNew ("user:/test/k5", KEY_END), KS_END);

	// Act
	ssize_t result = ksSubtract (total, sub);

	// Assert
	succeed_if_fmt (result == 2, "should have removed 2 keys (was %zu)", result);
	succeed_if (ksGetSize (total) == 0, "total should be empty");

	ksDel (total);
	ksDel (sub);
}

static void test_ksSubtract_3 (void)
{
	printf ("Testing ksSubtract (3)\n");

	// Arrange
	KeySet * total =
		ksNew (10, keyNew ("user:/test/k1", KEY_END), keyNew ("user:/test/k2", KEY_END), keyNew ("user:/test/k3", KEY_END),
		       keyNew ("user:/test/k4", KEY_END), keyNew ("user:/test/k5", KEY_END), keyNew ("user:/test/k6", KEY_END), KS_END);

	KeySet * sub =
		ksNew (10, keyNew ("user:/test/k2", KEY_END), keyNew ("user:/test/k3", KEY_END), keyNew ("user:/test/k5", KEY_END), KS_END);

	// Act
	ssize_t result = ksSubtract (total, sub);

	// Assert
	succeed_if_fmt (result == 3, "should have removed 3 keys (was %zu)", result);
	succeed_if (ksGetSize (total) == 3, "total should still have 3 keys");
	succeed_if (ksLookupByName (total, "user:/test/k1", 0) != NULL, "total should contain user:/test/k1");
	succeed_if (ksLookupByName (total, "user:/test/k4", 0) != NULL, "total should contain user:/test/k4");
	succeed_if (ksLookupByName (total, "user:/test/k6", 0) != NULL, "total should contain user:/test/k6");

	ksDel (total);
	ksDel (sub);
}

static void test_ksSubtract_4 (void)
{
	printf ("Testing ksSubtract (4)\n");

	// Arrange
	KeySet * total =
		ksNew (10, keyNew ("user:/test/b1", KEY_END), keyNew ("user:/test/c2", KEY_END), keyNew ("user:/test/d3", KEY_END),
		       keyNew ("user:/test/e4", KEY_END), keyNew ("user:/test/f5", KEY_END), keyNew ("user:/test/zz", KEY_END), KS_END);

	KeySet * sub = ksNew (10, keyNew ("user:/test/aa", KEY_END), keyNew ("user:/test/c2", KEY_END), keyNew ("user:/test/d1", KEY_END),
			      keyNew ("user:/test/e4", KEY_END), keyNew ("user:/test/f5", KEY_END), KS_END);

	// Act
	ssize_t result = ksSubtract (total, sub);

	// Assert
	succeed_if_fmt (result == 3, "should have removed 3 keys (was %zu)", result);
	succeed_if (ksGetSize (total) == 3, "total should still contain 3 keys");
	succeed_if (ksLookupByName (total, "user:/test/b1", 0) != NULL, "total should contain user:/test/k1");
	succeed_if (ksLookupByName (total, "user:/test/d3", 0) != NULL, "total should contain user:/test/k3");
	succeed_if (ksLookupByName (total, "user:/test/zz", 0) != NULL, "total should contain user:/test/zz");

	ksDel (total);
	ksDel (sub);
}

static void test_ksSubtract_5 (void)
{
	printf ("Testing ksSubtract (5)\n");

	// Arrange
	KeySet * total = ksNew (10, keyNew ("user:/test/a", KEY_END), keyNew ("user:/test/b", KEY_END), KS_END);

	KeySet * sub = ksNew (10, keyNew ("user:/test/c", KEY_END), keyNew ("user:/test/d", KEY_END), KS_END);

	// Act
	ssize_t result = ksSubtract (total, sub);

	// Assert
	succeed_if_fmt (result == 0, "should have removed 0 keys (was %zu)", result);
	succeed_if (ksGetSize (total) == 2, "total should still have 2 entries");

	ksDel (total);
	ksDel (sub);
}

int main (int argc, char ** argv)
{
	printf ("KS         TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_ksToArray ();
	test_ksRenameKeys ();
	test_cascadingLookup ();
	test_circularLinkLookup ();
	test_creatingLookup ();
	test_ksNoAlloc ();
	test_ksRename ();
	test_ksFindHierarchy ();
	test_ksSearch ();
	test_ksSubtract_nullParameters ();
	test_ksSubtract_emptyParameters ();
	test_ksSubtract_1 ();
	test_ksSubtract_2 ();
	test_ksSubtract_3 ();
	test_ksSubtract_4 ();
	test_ksSubtract_5 ();

	printf ("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
