/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbmerge.h>
#include <tests_internal.h>

static Key * addKeyToKeySet (Key * key, KeySet * ks)
{
	ksAppendKey (ks, key);
	return key;
}

static void test_new_keys_in_both (void)
{
	printf ("test new keys in both merge scenario\n");

	KeySet * ksBase = ksNew (0, KS_END);
	KeySet * ksOurs = ksNew (0, KS_END);
	KeySet * ksTheirs = ksNew (0, KS_END);

	Key * baseRoot = keyNew ("system:/test", KEY_END);
	Key * oursRoot = keyNew ("system:/test", KEY_END);
	Key * theirsRoot = keyNew ("system:/test", KEY_END);
	Key * root = keyNew ("system:/test", KEY_END);

	ksAppendKey (ksBase, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));

	ksAppendKey (ksOurs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksOurs, keyNew ("system:/test/k2", KEY_VALUE, "k2", KEY_END));

	ksAppendKey (ksTheirs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksTheirs, keyNew ("system:/test/k3", KEY_VALUE, "k3", KEY_END));

	Key * information = keyNew ("system:/", KEY_END);

	KeySet * result = elektraMerge (ksOurs, oursRoot, ksTheirs, theirsRoot, ksBase, baseRoot, root, MERGE_STRATEGY_ABORT, information);

	KeySet * trashcan = ksNew (0, KS_END);

	succeed_if (result != NULL, "result must not be NULL");
	succeed_if (ksGetSize (result) == 3, "result must contain 3 keys");
	succeed_if (ksLookupByName (result, "system:/test/k1", KDB_O_NONE) != NULL, "system:/test/k1 must be included in result");
	succeed_if (ksLookupByName (result, "system:/test/k2", KDB_O_NONE) != NULL, "system:/test/k2 must be included in result");
	succeed_if (ksLookupByName (result, "system:/test/k3", KDB_O_NONE) != NULL, "system:/test/k3 must be included in result");
	succeed_if (elektraMergeGetConflicts (information) == 0, "must not have any conflicts");

	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k1", KEY_END), trashcan)) ==
			    false,
		    "system:/test/k1 must not conflict");
	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k2", KEY_END), trashcan)) ==
			    false,
		    "system:/test/k2 must not conflict");
	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k3", KEY_END), trashcan)) ==
			    false,
		    "system:/test/k3 must not conflict");

	KeySet * conflicts = elektraMergeGetConflictingKeys (information, root);
	succeed_if (ksGetSize (conflicts) == 0, "conflicts must be empty")

		keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
	ksDel (conflicts);
	ksDel (trashcan);
}

static void test_removed_keys_in_both (void)
{
	printf ("test removed keys in both merge scenario\n");

	KeySet * ksBase = ksNew (0, KS_END);
	KeySet * ksOurs = ksNew (0, KS_END);
	KeySet * ksTheirs = ksNew (0, KS_END);

	Key * baseRoot = keyNew ("system:/test", KEY_END);
	Key * oursRoot = keyNew ("system:/test", KEY_END);
	Key * theirsRoot = keyNew ("system:/test", KEY_END);
	Key * root = keyNew ("system:/test", KEY_END);

	ksAppendKey (ksBase, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksBase, keyNew ("system:/test/k2", KEY_VALUE, "k2", KEY_END));
	ksAppendKey (ksBase, keyNew ("system:/test/k3", KEY_VALUE, "k3", KEY_END));

	// we removed k3
	ksAppendKey (ksOurs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksOurs, keyNew ("system:/test/k2", KEY_VALUE, "k2", KEY_END));

	// they removed k2
	ksAppendKey (ksTheirs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksTheirs, keyNew ("system:/test/k3", KEY_VALUE, "k3", KEY_END));

	Key * information = keyNew ("system:/", KEY_END);

	KeySet * result = elektraMerge (ksOurs, oursRoot, ksTheirs, theirsRoot, ksBase, baseRoot, root, MERGE_STRATEGY_ABORT, information);

	KeySet * trashcan = ksNew (0, KS_END);

	succeed_if (result != NULL, "result must not be NULL");
	succeed_if (ksGetSize (result) == 1, "result must contain 1 keys");
	succeed_if (ksLookupByName (result, "system:/test/k1", KDB_O_NONE) != NULL, "system:/test/k1 must be included in result");
	succeed_if (elektraMergeGetConflicts (information) == 0, "must not have any conflicts");

	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k1", KEY_END), trashcan)) ==
			    false,
		    "system:/test/k1 must not conflict");
	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k2", KEY_END), trashcan)) ==
			    false,
		    "system:/test/k2 must not conflict");
	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k3", KEY_END), trashcan)) ==
			    false,
		    "system:/test/k3 must not conflict");

	KeySet * conflicts = elektraMergeGetConflictingKeys (information, root);
	succeed_if (ksGetSize (conflicts) == 0, "conflicts must be empty")

		keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
	ksDel (conflicts);
	ksDel (trashcan);
}

static void test_changed_different_keys_in_both (void)
{
	printf ("test removed keys in both merge scenario\n");

	KeySet * ksBase = ksNew (0, KS_END);
	KeySet * ksOurs = ksNew (0, KS_END);
	KeySet * ksTheirs = ksNew (0, KS_END);

	Key * baseRoot = keyNew ("system:/test", KEY_END);
	Key * oursRoot = keyNew ("system:/test", KEY_END);
	Key * theirsRoot = keyNew ("system:/test", KEY_END);
	Key * root = keyNew ("system:/test", KEY_END);

	ksAppendKey (ksBase, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksBase, keyNew ("system:/test/k2", KEY_VALUE, "k2", KEY_END));
	ksAppendKey (ksBase, keyNew ("system:/test/k3", KEY_VALUE, "k3", KEY_END));

	// we changed k2
	ksAppendKey (ksOurs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksOurs, keyNew ("system:/test/k2", KEY_VALUE, "k2-c", KEY_END));
	ksAppendKey (ksOurs, keyNew ("system:/test/k3", KEY_VALUE, "k3", KEY_END));

	// they changed k3
	ksAppendKey (ksTheirs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksTheirs, keyNew ("system:/test/k2", KEY_VALUE, "k2", KEY_END));
	ksAppendKey (ksTheirs, keyNew ("system:/test/k3", KEY_VALUE, "k3-c", KEY_END));

	Key * information = keyNew ("system:/", KEY_END);

	KeySet * result = elektraMerge (ksOurs, oursRoot, ksTheirs, theirsRoot, ksBase, baseRoot, root, MERGE_STRATEGY_ABORT, information);

	KeySet * trashcan = ksNew (0, KS_END);

	succeed_if (result != NULL, "result must not be NULL");

	succeed_if (ksGetSize (result) == 3, "result must contain 3 keys");
	Key * k1 = ksLookupByName (result, "system:/test/k1", KDB_O_NONE);
	succeed_if (k1 != NULL, "system:/test/k1 must be included in result");
	succeed_if (strcmp ("k1", keyString (k1)) == 0, "value of k1 must be 'k1'");

	Key * k2 = ksLookupByName (result, "system:/test/k2", KDB_O_NONE);
	succeed_if (k2 != NULL, "system:/test/k2 must be included in result");
	succeed_if (strcmp ("k2-c", keyString (k2)) == 0, "value of k2 must be 'k2-c'");

	Key * k3 = ksLookupByName (result, "system:/test/k3", KDB_O_NONE);
	succeed_if (k3 != NULL, "system:/test/k3 must be included in result");
	succeed_if (strcmp ("k3-c", keyString (k3)) == 0, "value of k3 must be 'k3-c'");

	succeed_if (elektraMergeGetConflicts (information) == 0, "must not have any conflicts");

	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k1", KEY_END), trashcan)) ==
			    false,
		    "system:/test/k1 must not conflict");
	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k2", KEY_END), trashcan)) ==
			    false,
		    "system:/test/k2 must not conflict");
	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k3", KEY_END), trashcan)) ==
			    false,
		    "system:/test/k3 must not conflict");

	KeySet * conflicts = elektraMergeGetConflictingKeys (information, root);
	succeed_if (ksGetSize (conflicts) == 0, "conflicts must be empty")

		keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
	ksDel (conflicts);
	ksDel (trashcan);
}

static void test_changed_same_key_abort (void)
{
	printf ("test conflict with ABORT\n");

	KeySet * ksBase = ksNew (0, KS_END);
	KeySet * ksOurs = ksNew (0, KS_END);
	KeySet * ksTheirs = ksNew (0, KS_END);

	Key * baseRoot = keyNew ("system:/test", KEY_END);
	Key * oursRoot = keyNew ("system:/test", KEY_END);
	Key * theirsRoot = keyNew ("system:/test", KEY_END);
	Key * root = keyNew ("system:/test", KEY_END);

	ksAppendKey (ksBase, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksBase, keyNew ("system:/test/k2", KEY_VALUE, "k2", KEY_END));

	// we changed k2
	ksAppendKey (ksOurs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksOurs, keyNew ("system:/test/k2", KEY_VALUE, "k2-ours", KEY_END));

	// they changed k2
	ksAppendKey (ksTheirs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksTheirs, keyNew ("system:/test/k2", KEY_VALUE, "k2-theirs", KEY_END));

	Key * information = keyNew ("system:/", KEY_END);

	KeySet * result = elektraMerge (ksOurs, oursRoot, ksTheirs, theirsRoot, ksBase, baseRoot, root, MERGE_STRATEGY_ABORT, information);

	KeySet * trashcan = ksNew (0, KS_END);

	succeed_if (result == NULL, "result must be NULL");
	succeed_if (elektraMergeGetConflicts (information) == 1, "must have 1 conflicts");

	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k1", KEY_END), trashcan)) ==
			    false,
		    "system:/test/k1 must not conflict");
	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k2", KEY_END), trashcan)) ==
			    true,
		    "system:/test/k2 must conflict");

	KeySet * conflicts = elektraMergeGetConflictingKeys (information, root);
	succeed_if (ksGetSize (conflicts) == 1, "conflicts must contain single key");
	succeed_if (ksLookupByName (conflicts, "system:/test/k2", 0) != NULL, "conflicts must contain entry for system:/test/k2");

	keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
	ksDel (conflicts);
	ksDel (trashcan);
}

static void test_changed_same_key_their (void)
{
	printf ("test conflict with THEIR\n");

	KeySet * ksBase = ksNew (0, KS_END);
	KeySet * ksOurs = ksNew (0, KS_END);
	KeySet * ksTheirs = ksNew (0, KS_END);

	Key * baseRoot = keyNew ("system:/test", KEY_END);
	Key * oursRoot = keyNew ("system:/test", KEY_END);
	Key * theirsRoot = keyNew ("system:/test", KEY_END);
	Key * root = keyNew ("system:/test", KEY_END);

	ksAppendKey (ksBase, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksBase, keyNew ("system:/test/k2", KEY_VALUE, "k2", KEY_END));

	// we changed k2
	ksAppendKey (ksOurs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksOurs, keyNew ("system:/test/k2", KEY_VALUE, "k2-our", KEY_END));

	// they changed k2
	ksAppendKey (ksTheirs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksTheirs, keyNew ("system:/test/k2", KEY_VALUE, "k2-their", KEY_END));

	Key * information = keyNew ("system:/", KEY_END);

	KeySet * result = elektraMerge (ksOurs, oursRoot, ksTheirs, theirsRoot, ksBase, baseRoot, root, MERGE_STRATEGY_THEIR, information);

	KeySet * trashcan = ksNew (0, KS_END);

	succeed_if (result != NULL, "result must not be NULL");

	succeed_if (ksGetSize (result) == 2, "result must contain 2 keys");
	Key * k1 = ksLookupByName (result, "system:/test/k1", KDB_O_NONE);
	succeed_if (k1 != NULL, "system:/test/k1 must be included in result");
	succeed_if (strcmp ("k1", keyString (k1)) == 0, "value of k1 must be 'k1'");

	Key * k2 = ksLookupByName (result, "system:/test/k2", KDB_O_NONE);
	succeed_if (k2 != NULL, "system:/test/k2 must be included in result");
	succeed_if (strcmp ("k2-their", keyString (k2)) == 0, "value of k2 must be 'k2-their'");

	succeed_if (elektraMergeGetConflicts (information) == 1, "must have 1 conflicts");

	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k1", KEY_END), trashcan)) ==
			    false,
		    "system:/test/k1 must not conflict");
	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k2", KEY_END), trashcan)) ==
			    true,
		    "system:/test/k2 must conflict");

	KeySet * conflicts = elektraMergeGetConflictingKeys (information, root);
	succeed_if (ksGetSize (conflicts) == 1, "conflicts must contain single key");
	succeed_if (ksLookupByName (conflicts, "system:/test/k2", 0) != NULL, "conflicts must contain entry for system:/test/k2");

	keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
	ksDel (conflicts);
	ksDel (trashcan);
}

static void test_changed_same_key_our (void)
{
	printf ("test conflict with OUR\n");

	KeySet * ksBase = ksNew (0, KS_END);
	KeySet * ksOurs = ksNew (0, KS_END);
	KeySet * ksTheirs = ksNew (0, KS_END);

	Key * baseRoot = keyNew ("system:/test/base", KEY_END);
	Key * oursRoot = keyNew ("system:/test/our", KEY_END);
	Key * theirsRoot = keyNew ("system:/test/their", KEY_END);
	Key * root = keyNew ("system:/test/result", KEY_END);

	ksAppendKey (ksBase, keyNew ("system:/test/base/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksBase, keyNew ("system:/test/base/k2", KEY_VALUE, "k2", KEY_END));

	// we changed k2
	ksAppendKey (ksOurs, keyNew ("system:/test/our/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksOurs, keyNew ("system:/test/our/k2", KEY_VALUE, "k2-our", KEY_END));

	// they changed k2
	ksAppendKey (ksTheirs, keyNew ("system:/test/their/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksTheirs, keyNew ("system:/test/their/k2", KEY_VALUE, "k2-their", KEY_END));

	Key * information = keyNew ("system:/", KEY_END);

	KeySet * result = elektraMerge (ksOurs, oursRoot, ksTheirs, theirsRoot, ksBase, baseRoot, root, MERGE_STRATEGY_OUR, information);

	KeySet * trashcan = ksNew (0, KS_END);

	succeed_if (result != NULL, "result must not be NULL");

	succeed_if (ksGetSize (result) == 2, "result must contain 2 keys");
	Key * k1 = ksLookupByName (result, "system:/test/result/k1", KDB_O_NONE);
	succeed_if (k1 != NULL, "system:/test/result/k1 must be included in result");
	succeed_if (strcmp ("k1", keyString (k1)) == 0, "value of k1 must be 'k1'");

	Key * k2 = ksLookupByName (result, "system:/test/result/k2", KDB_O_NONE);
	succeed_if (k2 != NULL, "system:/test/result/k2 must be included in result");
	succeed_if (strcmp ("k2-our", keyString (k2)) == 0, "value of k2 must be 'k2-our'");

	succeed_if (elektraMergeGetConflicts (information) == 1, "must have 1 conflicts");

	succeed_if (elektraMergeIsKeyConflicting (information, root,
						  addKeyToKeySet (keyNew ("system:/test/result/k1", KEY_END), trashcan)) == false,
		    "system:/test/result/k1 must not conflict");
	succeed_if (elektraMergeIsKeyConflicting (information, root,
						  addKeyToKeySet (keyNew ("system:/test/result/k2", KEY_END), trashcan)) == true,
		    "system:/test/result/k2 must conflict");

	KeySet * conflicts = elektraMergeGetConflictingKeys (information, oursRoot);
	succeed_if (ksGetSize (conflicts) == 1, "conflicts must contain single key");
	succeed_if (ksLookupByName (conflicts, "system:/test/our/k2", 0) != NULL, "conflicts must contain entry for system:/test/our/k2");

	keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
	ksDel (conflicts);
	ksDel (trashcan);
}

static void test_changed_same_key_our_cascading_root (void)
{
	printf ("test conflict with OUR with cascading root\n");

	KeySet * ksBase = ksNew (0, KS_END);
	KeySet * ksOurs = ksNew (0, KS_END);
	KeySet * ksTheirs = ksNew (0, KS_END);

	Key * baseRoot = keyNew ("/test/base", KEY_END);
	Key * oursRoot = keyNew ("/test/our", KEY_END);
	Key * theirsRoot = keyNew ("/test/their", KEY_END);
	Key * root = keyNew ("/test/result", KEY_END);

	ksAppendKey (ksBase, keyNew ("system:/test/base/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksBase, keyNew ("system:/test/base/k2", KEY_VALUE, "k2", KEY_END));

	// we changed k2
	ksAppendKey (ksOurs, keyNew ("system:/test/our/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksOurs, keyNew ("system:/test/our/k2", KEY_VALUE, "k2-our", KEY_END));

	// they changed k2
	ksAppendKey (ksTheirs, keyNew ("system:/test/their/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksTheirs, keyNew ("system:/test/their/k2", KEY_VALUE, "k2-their", KEY_END));

	Key * information = keyNew ("system:/", KEY_END);

	KeySet * result = elektraMerge (ksOurs, oursRoot, ksTheirs, theirsRoot, ksBase, baseRoot, root, MERGE_STRATEGY_OUR, information);

	KeySet * trashcan = ksNew (0, KS_END);

	succeed_if (result != NULL, "result must not be NULL");

	succeed_if (ksGetSize (result) == 2, "result must contain 2 keys");
	Key * k1 = ksLookupByName (result, "system:/test/result/k1", KDB_O_NONE);
	succeed_if (k1 != NULL, "system:/test/result/k1 must be included in result");
	succeed_if (strcmp ("k1", keyString (k1)) == 0, "value of k1 must be 'k1'");

	Key * k2 = ksLookupByName (result, "system:/test/result/k2", KDB_O_NONE);
	succeed_if (k2 != NULL, "system:/test/result/k2 must be included in result");
	succeed_if (strcmp ("k2-our", keyString (k2)) == 0, "value of k2 must be 'k2-our'");

	succeed_if (elektraMergeGetConflicts (information) == 1, "must have 1 conflicts");

	succeed_if (elektraMergeIsKeyConflicting (information, root,
						  addKeyToKeySet (keyNew ("system:/test/result/k1", KEY_END), trashcan)) == false,
		    "system:/test/result/k1 must not conflict");
	succeed_if (elektraMergeIsKeyConflicting (information, root,
						  addKeyToKeySet (keyNew ("system:/test/result/k2", KEY_END), trashcan)) == true,
		    "system:/test/result/k2 must conflict");

	KeySet * conflicts = elektraMergeGetConflictingKeys (information, oursRoot);
	succeed_if (ksGetSize (conflicts) == 1, "conflicts must contain single key");
	succeed_if (ksLookupByName (conflicts, "system:/test/our/k2", 0) != NULL, "conflicts must contain entry for system:/test/our/k2");

	keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
	ksDel (conflicts);
	ksDel (trashcan);
}

static void test_changed_same_key_their_cascading_root (void)
{
	printf ("test conflict with THEIR with cascading root\n");

	KeySet * ksBase = ksNew (0, KS_END);
	KeySet * ksOurs = ksNew (0, KS_END);
	KeySet * ksTheirs = ksNew (0, KS_END);

	Key * baseRoot = keyNew ("/test", KEY_END);
	Key * oursRoot = keyNew ("/test", KEY_END);
	Key * theirsRoot = keyNew ("/test", KEY_END);
	Key * root = keyNew ("/test", KEY_END);

	ksAppendKey (ksBase, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksBase, keyNew ("system:/test/k2", KEY_VALUE, "k2", KEY_END));

	// we changed k2
	ksAppendKey (ksOurs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksOurs, keyNew ("system:/test/k2", KEY_VALUE, "k2-our", KEY_END));

	// they changed k2
	ksAppendKey (ksTheirs, keyNew ("system:/test/k1", KEY_VALUE, "k1", KEY_END));
	ksAppendKey (ksTheirs, keyNew ("system:/test/k2", KEY_VALUE, "k2-their", KEY_END));

	Key * information = keyNew ("system:/", KEY_END);

	KeySet * result = elektraMerge (ksOurs, oursRoot, ksTheirs, theirsRoot, ksBase, baseRoot, root, MERGE_STRATEGY_THEIR, information);

	KeySet * trashcan = ksNew (0, KS_END);

	succeed_if (result != NULL, "result must not be NULL");

	succeed_if (ksGetSize (result) == 2, "result must contain 2 keys");
	Key * k1 = ksLookupByName (result, "system:/test/k1", KDB_O_NONE);
	succeed_if (k1 != NULL, "system:/test/k1 must be included in result");
	succeed_if (strcmp ("k1", keyString (k1)) == 0, "value of k1 must be 'k1'");

	Key * k2 = ksLookupByName (result, "system:/test/k2", KDB_O_NONE);
	succeed_if (k2 != NULL, "system:/test/k2 must be included in result");
	succeed_if (strcmp ("k2-their", keyString (k2)) == 0, "value of k2 must be 'k2-their'");

	succeed_if (elektraMergeGetConflicts (information) == 1, "must have 1 conflicts");

	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k1", KEY_END), trashcan)) ==
			    false,
		    "system:/test/k1 must not conflict");
	succeed_if (elektraMergeIsKeyConflicting (information, root, addKeyToKeySet (keyNew ("system:/test/k2", KEY_END), trashcan)) ==
			    true,
		    "system:/test/k2 must conflict");

	KeySet * conflicts = elektraMergeGetConflictingKeys (information, root);
	succeed_if (ksGetSize (conflicts) == 1, "conflicts must contain single key");
	succeed_if (ksLookupByName (conflicts, "system:/test/k2", 0) != NULL, "conflicts must contain entry for system:/test/k2");

	keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
	ksDel (conflicts);
	ksDel (trashcan);
}

int main (int argc, char ** argv)
{
	printf ("MERGE       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_new_keys_in_both ();
	test_removed_keys_in_both ();
	test_changed_different_keys_in_both ();
	test_changed_same_key_abort ();
	test_changed_same_key_their ();
	test_changed_same_key_our ();
	test_changed_same_key_our_cascading_root ();
	test_changed_same_key_their_cascading_root ();

	printf ("\ntest_merge RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
