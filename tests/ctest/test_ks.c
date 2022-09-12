/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbease.h>
#include <tests_internal.h>

ssize_t elektraKeysetCopyInternal (ElektraKeyset * ks, size_t to, size_t from);

static void test_ksRenameKeys (void)
{
	printf ("test rename keys\n");
	ElektraKeyset * ks = elektraKeysetNew (20, elektraKeyNew ("system:/some/common/prefix", ELEKTRA_KEY_END), elektraKeyNew ("system:/some/common/prefix/dir", ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/some/common/prefix/dir/keya", ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/some/common/prefix/some", ELEKTRA_KEY_VALUE, "huhu", ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/some/common/prefix/other", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * cmp = elektraKeysetNew (20, elektraKeyNew ("user:/x/dir", ELEKTRA_KEY_END), elektraKeyNew ("user:/x/dir/keya", ELEKTRA_KEY_END),
			      elektraKeyNew ("user:/x/some", ELEKTRA_KEY_VALUE, "huhu", ELEKTRA_KEY_END), elektraKeyNew ("user:/x/other", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * result = elektraKeysetRenameKeys (ks, "user:/x");
	compare_keyset (result, cmp);
	// output_keyset(result);
	elektraKeysetDel (cmp);
	elektraKeysetDel (result);
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	result = elektraKeysetRenameKeys (ks, "user");
	output_keyset (result);

	elektraKeysetDel (result);
	elektraKeysetDel (ks);
}

static void test_cascadingLookup (void)
{
	printf ("test cascading lookup\n");
	ElektraKey * k0;
	ElektraKey * k1;
	ElektraKey * k2;
	ElektraKey * k3;
	ElektraKeyset * ks =
		elektraKeysetNew (10, k0 = elektraKeyNew ("system:/benchmark/override/#0", ELEKTRA_KEY_END), k1 = elektraKeyNew ("system:/benchmark/override/#1", ELEKTRA_KEY_END),
		       k2 = elektraKeyNew ("user:/benchmark/override/#2", ELEKTRA_KEY_END), k3 = elektraKeyNew ("user:/benchmark/override/#3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKey * search = elektraKeyNew ("/benchmark/override/#0", ELEKTRA_KEY_END);
	ElektraKey * found = elektraKeysetLookup (ks, search, 0);
	succeed_if (found == k0, "found wrong key");

	elektraKeySetName (search, "/benchmark/override/#1");
	found = elektraKeysetLookup (ks, search, 0);
	succeed_if (found == k1, "found wrong key");
	elektraKeyDel (search);

	search = elektraKeyNew ("/benchmark/override/#2", ELEKTRA_KEY_END);
	found = elektraKeysetLookup (ks, search, 0);
	succeed_if (found == k2, "found wrong key");

	elektraKeySetName (search, "/benchmark/override/#3");
	found = elektraKeysetLookup (ks, search, 0);
	succeed_if (found == k3, "found wrong key");
	elektraKeyDel (search);
	elektraKeysetDel (ks);
}

static void test_creatingLookup (void)
{
	printf ("Test creating lookup\n");

	ElektraKeyset * ks = elektraKeysetNew (10, ELEKTRA_KS_END);

	ElektraKey * searchKey = elektraKeyNew ("user:/something", ELEKTRA_KEY_VALUE, "a value", ELEKTRA_KEY_END);
	ElektraKey * k0 = elektraKeysetLookup (ks, searchKey, ELEKTRA_KDB_O_CREATE);
	exit_if_fail (k0, "no key was created");
	succeed_if_same_string (elektraKeyName (k0), elektraKeyName (searchKey));
	succeed_if_same_string (elektraKeyString (k0), elektraKeyString (searchKey));

	ElektraKey * k1 = elektraKeysetLookup (ks, searchKey, ELEKTRA_KDB_O_CREATE);
	exit_if_fail (k1, "no key was returned");
	succeed_if (k0 == k1, "not the same key");

	elektraKeyDel (searchKey);
	elektraKeysetDel (ks);


	ks = elektraKeysetNew (10, ELEKTRA_KS_END);

	searchKey = elektraKeyNew ("dir:/something", ELEKTRA_KEY_VALUE, "a value", ELEKTRA_KEY_END);
	k0 = elektraKeysetLookup (ks, searchKey, ELEKTRA_KDB_O_CREATE);
	exit_if_fail (k0, "no key was created");
	succeed_if_same_string (elektraKeyName (k0), elektraKeyName (searchKey));
	succeed_if_same_string (elektraKeyString (k0), elektraKeyString (searchKey));

	k1 = elektraKeysetLookup (ks, searchKey, ELEKTRA_KDB_O_CREATE);
	exit_if_fail (k1, "no key was returned");
	succeed_if (k0 == k1, "not the same key");

	elektraKeyDel (searchKey);
	elektraKeysetDel (ks);


	ks = elektraKeysetNew (10, ELEKTRA_KS_END);

	searchKey = elektraKeyNew ("/something", ELEKTRA_KEY_VALUE, "a value", ELEKTRA_KEY_END);

	// check if duplication works:
	ElektraKey * dupKey = elektraKeyDup (searchKey, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dupKey), elektraKeyName (searchKey));
	succeed_if_same_string (elektraKeyString (dupKey), elektraKeyString (searchKey));
	elektraKeysetAppendKey (ks, dupKey);

	k0 = elektraKeysetLookup (ks, searchKey, ELEKTRA_KDB_O_CREATE);
	exit_if_fail (k0, "no key was created");
	succeed_if_same_string (elektraKeyName (k0), elektraKeyName (searchKey));
	succeed_if_same_string (elektraKeyString (k0), elektraKeyString (searchKey));

	k1 = elektraKeysetLookup (ks, searchKey, ELEKTRA_KDB_O_CREATE);
	exit_if_fail (k1, "no key was returned");
	succeed_if (k0 == k1, "not the same key");

	elektraKeyDel (searchKey);
	elektraKeysetDel (ks);


	ks = elektraKeysetNew (10, ELEKTRA_KS_END);

	searchKey = elektraKeyNew ("proc:/something", ELEKTRA_KEY_VALUE, "a value", ELEKTRA_KEY_END);
	k0 = elektraKeysetLookup (ks, searchKey, ELEKTRA_KDB_O_CREATE);
	exit_if_fail (k0, "no key was created");
	succeed_if_same_string (elektraKeyName (k0), elektraKeyName (searchKey));
	succeed_if_same_string (elektraKeyString (k0), elektraKeyString (searchKey));

	k1 = elektraKeysetLookup (ks, searchKey, ELEKTRA_KDB_O_CREATE);
	exit_if_fail (k1, "no key was returned");
	succeed_if (k0 == k1, "not the same key");

	elektraKeyDel (searchKey);
	elektraKeysetDel (ks);
}


static void test_ksToArray (void)
{
	printf ("Test ksToArray\n");

	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew ("user:/test1", ELEKTRA_KEY_END), elektraKeyNew ("user:/test2", ELEKTRA_KEY_END), elektraKeyNew ("user:/test3", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKey ** keyArray = calloc (elektraKeysetGetSize (ks), sizeof (ElektraKey *));
	elektraKsToMemArray (ks, keyArray);

	succeed_if_same_string ("user:/test1", elektraKeyName (keyArray[0]));
	succeed_if_same_string ("user:/test2", elektraKeyName (keyArray[1]));
	succeed_if_same_string ("user:/test3", elektraKeyName (keyArray[2]));

	/* test if cursor is restored */
	elektraKeysetNext (ks);
	elektraCursor cursor = elektraKeysetGetCursor (ks);
	elektraKsToMemArray (ks, keyArray);

	succeed_if (elektraKeysetGetCursor (ks) == cursor, "cursor was not restored");

	succeed_if (elektraKsToMemArray (0, keyArray) < 0, "wrong result on null pointer");
	succeed_if (elektraKsToMemArray (ks, 0) < 0, "wrong result on null buffer");
	ElektraKeyset * empty = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (elektraKsToMemArray (empty, keyArray) == 0, "wrong result on empty keyset");
	elektraKeysetDel (empty);

	elektraFree (keyArray);
	elektraKeysetDel (ks);
}

static void test_ksNoAlloc (void)
{
	printf ("Test no alloc\n");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (ks->alloc == 0, "alloc is not 0");
	succeed_if (ks->size == 0, "size is not 0");
	succeed_if (ks->array == NULL, "array is not NULL");

	elektraKeysetDel (ks);

	ks = elektraKeysetNew (1, ELEKTRA_KS_END);

	succeed_if (ks->alloc != 0, "alloc is 0");
	succeed_if (ks->size == 0, "size is not 0");
	succeed_if (ks->array != NULL, "array is NULL");

	elektraKeysetDel (ks);
}

static void test_ksRename (void)
{
	printf ("Test ksRename\n");

	ElektraKey * key1 = elektraKeyNew ("system:/baz", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_END);
	ElektraKey * key2 = elektraKeyNew ("system:/baz/bar", ELEKTRA_KEY_VALUE, "6", ELEKTRA_KEY_END);
	ElektraKey * key3 = elektraKeyNew ("system:/baz/bar/bar", ELEKTRA_KEY_VALUE, "7", ELEKTRA_KEY_END);
	ElektraKey * key4 = elektraKeyNew ("system:/baz/bar/foo", ELEKTRA_KEY_VALUE, "8", ELEKTRA_KEY_END);

	ElektraKeyset * ks = elektraKeysetNew (
		24,
		// clang-format off
		elektraKeyNew ("system:/bar", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), 
		elektraKeyNew ("system:/bar/bar", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/bar/bar/bar", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/bar/bar/foo", ELEKTRA_KEY_VALUE, "4", ELEKTRA_KEY_END),
		key1,
		key2,
		key3,
		key4,
		elektraKeyNew ("system:/foo", ELEKTRA_KEY_VALUE, "9", ELEKTRA_KEY_END), 
		elektraKeyNew ("system:/foo/bar", ELEKTRA_KEY_VALUE, "10", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/foo/bar/bar", ELEKTRA_KEY_VALUE, "11", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/foo/bar/foo", ELEKTRA_KEY_VALUE, "12", ELEKTRA_KEY_END),
		// clang-format on
		ELEKTRA_KS_END);

	ElektraKey * keyRenamed1 = elektraKeyNew ("dir:/baz", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_END);
	ElektraKey * keyRenamed2 = elektraKeyNew ("dir:/baz/bar", ELEKTRA_KEY_VALUE, "6", ELEKTRA_KEY_END);
	ElektraKey * keyRenamed3 = elektraKeyNew ("dir:/baz/bar/bar", ELEKTRA_KEY_VALUE, "7", ELEKTRA_KEY_END);
	ElektraKey * keyRenamed4 = elektraKeyNew ("dir:/baz/bar/foo", ELEKTRA_KEY_VALUE, "8", ELEKTRA_KEY_END);

	ElektraKeyset * renamed =
		elektraKeysetNew (24,
		       // clang-format off
		       keyRenamed1, 
		       keyRenamed2,
		       keyRenamed3,
		       keyRenamed4,
		       elektraKeyNew ("system:/bar", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), 
		       elektraKeyNew ("system:/bar/bar", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/bar/bar/bar", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/bar/bar/foo", ELEKTRA_KEY_VALUE, "4", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/foo", ELEKTRA_KEY_VALUE, "9", ELEKTRA_KEY_END), 
		       elektraKeyNew ("system:/foo/bar", ELEKTRA_KEY_VALUE, "10", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/foo/bar/bar", ELEKTRA_KEY_VALUE, "11", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/foo/bar/foo", ELEKTRA_KEY_VALUE, "12", ELEKTRA_KEY_END),
		       // clang-format on
		       ELEKTRA_KS_END);

	ElektraKey * orig1 = elektraKeyNew ("system:/baz", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_END);
	ElektraKey * orig2 = elektraKeyNew ("system:/baz/bar", ELEKTRA_KEY_VALUE, "6", ELEKTRA_KEY_END);
	ElektraKey * orig3 = elektraKeyNew ("system:/baz/bar/bar", ELEKTRA_KEY_VALUE, "7", ELEKTRA_KEY_END);
	ElektraKey * orig4 = elektraKeyNew ("system:/baz/bar/foo", ELEKTRA_KEY_VALUE, "8", ELEKTRA_KEY_END);

	ElektraKeyset * orig = elektraKeysetDeepDup (ks);

	ElektraKey * root = elektraKeyNew ("user:/baz", ELEKTRA_KEY_END);
	ElektraKey * newRoot = elektraKeyNew ("user:/baz", ELEKTRA_KEY_END);

	succeed_if (ksRename (NULL, root, newRoot) == -1, "shouldn't accept NULL pointers");
	succeed_if (ksRename (ks, NULL, newRoot) == -1, "shouldn't accept NULL pointers");
	succeed_if (ksRename (ks, root, NULL) == -1, "shouldn't accept NULL pointers");

	elektraKeySetName (root, "/baz");
	succeed_if (ksRename (ks, root, newRoot) == -1, "shouldn't accept cascading keys");
	compare_keyset (ks, orig);

	elektraKeySetName (root, "user:/baz");
	elektraKeySetName (newRoot, "/baz");
	succeed_if (ksRename (ks, root, newRoot) == -1, "shouldn't accept cascading keys");
	compare_keyset (ks, orig);

	elektraKeySetName (root, "system:/zzzz");
	elektraKeySetName (newRoot, "dir:/baz");
	succeed_if (ksRename (ks, root, newRoot) == 0, "root not found should be nop");
	compare_keyset (ks, orig);

	elektraKeySetName (root, "system:/zzzz");
	elektraKeySetName (newRoot, "system:/baz");
	succeed_if (ksRename (ks, root, newRoot) == 0, "root not found should be nop");
	compare_keyset (ks, orig);

	elektraKeySetName (root, "system:/baz");
	elektraKeySetName (newRoot, "system:/baz");
	succeed_if (ksRename (ks, root, newRoot) == 4, "same root should always work");
	compare_keyset (ks, orig);

	elektraKeySetName (root, "system:/baz");
	elektraKeySetName (newRoot, "dir:/baz");
	// keys only referenced by ks
	succeed_if (ksRename (ks, root, newRoot) == 4, "didn't rename correctly");
	compare_keyset (ks, renamed);
	succeed_if (elektraKeyCmp (key1, keyRenamed1) == 0, "should have renamed in-place");
	succeed_if (elektraKeyCmp (key2, keyRenamed2) == 0, "should have renamed in-place");
	succeed_if (elektraKeyCmp (key3, keyRenamed3) == 0, "should have renamed in-place");
	succeed_if (elektraKeyCmp (key4, keyRenamed4) == 0, "should have renamed in-place");

	elektraKeysetDel (ks);
	ks = elektraKeysetDup (orig);
	key1 = elektraKeysetLookup (ks, orig1, 0);
	key2 = elektraKeysetLookup (ks, orig2, 0);
	key3 = elektraKeysetLookup (ks, orig3, 0);
	key4 = elektraKeysetLookup (ks, orig4, 0);

	elektraKeySetName (root, "system:/baz");
	elektraKeySetName (newRoot, "dir:/baz");
	// keys referenced by ks and orig
	succeed_if (ksRename (ks, root, newRoot) == 4, "didn't rename correctly");
	compare_keyset (ks, renamed);
	succeed_if (elektraKeyCmp (key1, orig1) == 0, "should have dup'ed key");
	succeed_if (elektraKeyCmp (key2, orig2) == 0, "should have dup'ed key");
	succeed_if (elektraKeyCmp (key3, orig3) == 0, "should have dup'ed key");
	succeed_if (elektraKeyCmp (key4, orig4) == 0, "should have dup'ed key");

	elektraKeysetDel (renamed);
	renamed = elektraKeysetNew (24,
			 // clang-format off
			 elektraKeyNew ("system:/bar", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), 
			 elektraKeyNew ("system:/bar/bar", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/bar/bar/bar", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/bar/bar/foo", ELEKTRA_KEY_VALUE, "4", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/baz/baz", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/baz/baz/bar", ELEKTRA_KEY_VALUE, "6", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/baz/baz/bar/bar", ELEKTRA_KEY_VALUE, "7", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/baz/baz/bar/foo", ELEKTRA_KEY_VALUE, "8", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/foo", ELEKTRA_KEY_VALUE, "9", ELEKTRA_KEY_END), 
			 elektraKeyNew ("system:/foo/bar", ELEKTRA_KEY_VALUE, "10", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/foo/bar/bar", ELEKTRA_KEY_VALUE, "11", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/foo/bar/foo", ELEKTRA_KEY_VALUE, "12", ELEKTRA_KEY_END),
			 // clang-format on
			 ELEKTRA_KS_END);

	elektraKeysetDel (ks);
	ks = elektraKeysetDup (orig);
	elektraKeySetName (root, "system:/baz");
	elektraKeySetName (newRoot, "system:/baz/baz");
	succeed_if (ksRename (ks, root, newRoot) == 4, "didn't rename correctly");
	compare_keyset (ks, renamed);

	elektraKeysetDel (renamed);
	renamed = elektraKeysetNew (24,
			 // clang-format off
			 elektraKeyNew ("system:/bar", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_END), 
			 elektraKeyNew ("system:/bar/bar", ELEKTRA_KEY_VALUE, "6", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/bar/bar/bar", ELEKTRA_KEY_VALUE, "7", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/bar/bar/foo", ELEKTRA_KEY_VALUE, "8", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/foo", ELEKTRA_KEY_VALUE, "9", ELEKTRA_KEY_END), 
			 elektraKeyNew ("system:/foo/bar", ELEKTRA_KEY_VALUE, "10", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/foo/bar/bar", ELEKTRA_KEY_VALUE, "11", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/foo/bar/foo", ELEKTRA_KEY_VALUE, "12", ELEKTRA_KEY_END),
			 // clang-format on
			 ELEKTRA_KS_END);

	elektraKeysetDel (ks);
	ks = elektraKeysetDup (orig);
	elektraKeySetName (root, "system:/baz");
	elektraKeySetName (newRoot, "system:/bar");
	succeed_if (ksRename (ks, root, newRoot) == 4, "should work even with existing keys below newRoot");
	compare_keyset (ks, renamed);

	elektraKeysetDel (renamed);
	renamed = elektraKeysetNew (24,
			 // clang-format off
			 elektraKeyNew ("system:/", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), 
			 elektraKeyNew ("system:/bar", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/bar/bar", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/bar/foo", ELEKTRA_KEY_VALUE, "4", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/baz", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/baz/bar", ELEKTRA_KEY_VALUE, "6", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/baz/bar/bar", ELEKTRA_KEY_VALUE, "7", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/baz/bar/foo", ELEKTRA_KEY_VALUE, "8", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/foo", ELEKTRA_KEY_VALUE, "9", ELEKTRA_KEY_END), 
			 elektraKeyNew ("system:/foo/bar", ELEKTRA_KEY_VALUE, "10", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/foo/bar/bar", ELEKTRA_KEY_VALUE, "11", ELEKTRA_KEY_END),
			 elektraKeyNew ("system:/foo/bar/foo", ELEKTRA_KEY_VALUE, "12", ELEKTRA_KEY_END),
			 // clang-format on
			 ELEKTRA_KS_END);

	elektraKeysetDel (ks);
	ks = elektraKeysetDup (orig);
	elektraKeySetName (root, "system:/bar");
	elektraKeySetName (newRoot, "system:/");
	succeed_if (ksRename (ks, root, newRoot) == 4, "didn't rename correctly");
	compare_keyset (ks, renamed);

	elektraKeysetDel (ks);
	elektraKeysetDel (orig);
	elektraKeysetDel (renamed);
	elektraKeyDel (orig1);
	elektraKeyDel (orig2);
	elektraKeyDel (orig3);
	elektraKeyDel (orig4);
	elektraKeyDel (root);
	elektraKeyDel (newRoot);
}

void test_ksFindHierarchy (void)
{
	printf ("Test ksFindHierarchy\n");

	ElektraKeyset * ks =
		elektraKeysetNew (24,
		       // clang-format off
		       elektraKeyNew ("system:/bar", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), 
		       elektraKeyNew ("system:/bar/bar", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/bar/bar/bar", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/bar/bar/foo", ELEKTRA_KEY_VALUE, "4", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/baz", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/baz/bar", ELEKTRA_KEY_VALUE, "6", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/baz/bar/bar", ELEKTRA_KEY_VALUE, "7", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/baz/bar/foo", ELEKTRA_KEY_VALUE, "8", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/foo", ELEKTRA_KEY_VALUE, "9", ELEKTRA_KEY_END), 
		       elektraKeyNew ("system:/foo/bar", ELEKTRA_KEY_VALUE, "10", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/foo/bar/bar", ELEKTRA_KEY_VALUE, "11", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/foo/bar/foo", ELEKTRA_KEY_VALUE, "12", ELEKTRA_KEY_END),
		       // clang-format on
		       ELEKTRA_KS_END);

	ElektraKey * root = elektraKeyNew ("/", ELEKTRA_KEY_END);

	elektraCursor end;

	succeed_if (elektraKeysetFindHierarchy (ks, NULL, &end) == -1, "shouldn't accept NULL");
	succeed_if (elektraKeysetFindHierarchy (NULL, root, &end) == -1, "shouldn't accept NULL");

	succeed_if (elektraKeysetFindHierarchy (ks, root, &end) == elektraKeysetGetSize (ks), "shouldn't find key");

	elektraKeySetName (root, "user:/");
	succeed_if (elektraKeysetFindHierarchy (ks, root, &end) == elektraKeysetGetSize (ks), "shouldn't find key");

	elektraKeySetName (root, "system:/zoo");
	succeed_if (elektraKeysetFindHierarchy (ks, root, &end) == elektraKeysetGetSize (ks), "shouldn't find key");

	elektraKeySetName (root, "system:/bar/foo");
	succeed_if (elektraKeysetFindHierarchy (ks, root, &end) == elektraKeysetGetSize (ks), "shouldn't find key");

	elektraKeySetName (root, "system:/");
	succeed_if (elektraKeysetFindHierarchy (ks, root, &end) == 0 && end == 12, "hierarchy should be present");

	elektraKeySetName (root, "system:/bar");
	succeed_if (elektraKeysetFindHierarchy (ks, root, &end) == 0 && end == 4, "hierarchy should be present");

	elektraKeySetName (root, "system:/bar/bar");
	succeed_if (elektraKeysetFindHierarchy (ks, root, &end) == 1 && end == 4, "hierarchy should be present");

	elektraKeySetName (root, "system:/baz/bar/bar");
	succeed_if (elektraKeysetFindHierarchy (ks, root, &end) == 6 && end == 7, "hierarchy should be present");

	elektraKeySetName (root, "system:/baz/bar/bar");
	succeed_if (elektraKeysetFindHierarchy (ks, root, NULL) == 6, "should accept NULL for end");

	elektraKeyDel (root);
	elektraKeysetDel (ks);
}

static ElektraKeyset * set_a (void)
{
	return elektraKeysetNew (16, elektraKeyNew ("user:/0", ELEKTRA_KEY_END), elektraKeyNew ("user:/a", ELEKTRA_KEY_END), elektraKeyNew ("user:/a/a", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/a/a/a", ELEKTRA_KEY_END), elektraKeyNew ("user:/a/a/b", ELEKTRA_KEY_END), elektraKeyNew ("user:/a/b", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/a/b/a", ELEKTRA_KEY_END), elektraKeyNew ("user:/a/b/b", ELEKTRA_KEY_END), elektraKeyNew ("user:/a/c", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/a/d", ELEKTRA_KEY_END), elektraKeyNew ("user:/a/x/a", ELEKTRA_KEY_END), elektraKeyNew ("user:/a/x/b", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/a/x/c", ELEKTRA_KEY_END), elektraKeyNew ("user:/a/x/c/a", ELEKTRA_KEY_END), elektraKeyNew ("user:/a/x/c/b", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/x", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static void test_ksSearch (void)
{
	printf ("Testing ksSearch\n");

	ElektraKeyset * a = set_a ();
	ElektraKey * s = elektraKeyNew ("user:/a", ELEKTRA_KEY_END);
	ssize_t result;

	elektraKeySetName (s, "user:/0");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 0, "insertpos wrong");

	elektraKeySetName (s, "user:/a");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 1, "insertpos wrong");

	elektraKeySetName (s, "user:/a/0");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == -3, "insertpos wrong");

	elektraKeySetName (s, "user:/a/a");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 2, "insertpos wrong");

	elektraKeySetName (s, "user:/a/a/a");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 3, "insertpos wrong");

	elektraKeySetName (s, "user:/a/a/b");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 4, "insertpos wrong");

	elektraKeySetName (s, "user:/a/b");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 5, "insertpos wrong");

	elektraKeySetName (s, "user:/a/b/a");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 6, "insertpos wrong");

	elektraKeySetName (s, "user:/a/b/b");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 7, "insertpos wrong");

	elektraKeySetName (s, "user:/a/c");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 8, "insertpos wrong");

	elektraKeySetName (s, "user:/a/d");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 9, "insertpos wrong");

	elektraKeySetName (s, "user:/a/x");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == -11, "insertpos wrong");

	elektraKeySetName (s, "user:/a/x/a");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 10, "insertpos wrong");

	elektraKeySetName (s, "user:/a/x/b");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 11, "insertpos wrong");

	elektraKeySetName (s, "user:/a/x/c");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 12, "insertpos wrong");

	elektraKeySetName (s, "user:/a/x/c/a");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 13, "insertpos wrong");

	elektraKeySetName (s, "user:/a/x/c/b");
	result = elektraKeysetSearch (a, s);
	succeed_if (result == 14, "insertpos wrong");

	elektraKeySetName (s, "user:/x");
	result = elektraKeysetSearch (a, s);
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

	elektraKeyDel (s);
	elektraKeysetDel (a);
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
	test_ksFindHierarchy ();
	test_ksSearch ();

	printf ("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
