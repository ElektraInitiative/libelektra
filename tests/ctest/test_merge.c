/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdbmerge.h>
#include <tests_internal.h>

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

	succeed_if (result != NULL, "result must not be NULL");
	succeed_if (ksGetSize (result) == 3, "result must contain 3 keys");
	succeed_if (ksLookupByName (result, "system:/test/k1", KDB_O_NONE) != NULL, "system:/test/k1 must be included in result");
	succeed_if (ksLookupByName (result, "system:/test/k2", KDB_O_NONE) != NULL, "system:/test/k2 must be included in result");
	succeed_if (ksLookupByName (result, "system:/test/k3", KDB_O_NONE) != NULL, "system:/test/k3 must be included in result");
	succeed_if (elektraMergeGetConflicts (information) == 0, "must not have any conflicts");

	keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
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

	succeed_if (result != NULL, "result must not be NULL");
	succeed_if (ksGetSize (result) == 1, "result must contain 1 keys");
	succeed_if (ksLookupByName (result, "system:/test/k1", KDB_O_NONE) != NULL, "system:/test/k1 must be included in result");
	succeed_if (elektraMergeGetConflicts (information) == 0, "must not have any conflicts");

	keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
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

	keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
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

	succeed_if (result == NULL, "result must be NULL");
	succeed_if (elektraMergeGetConflicts (information) == 1, "must have 1 conflicts");

	keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
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

	succeed_if (result != NULL, "result must not be NULL");

	succeed_if (ksGetSize (result) == 2, "result must contain 2 keys");
	Key * k1 = ksLookupByName (result, "system:/test/k1", KDB_O_NONE);
	succeed_if (k1 != NULL, "system:/test/k1 must be included in result");
	succeed_if (strcmp ("k1", keyString (k1)) == 0, "value of k1 must be 'k1'");

	Key * k2 = ksLookupByName (result, "system:/test/k2", KDB_O_NONE);
	succeed_if (k2 != NULL, "system:/test/k2 must be included in result");
	succeed_if (strcmp ("k2-their", keyString (k2)) == 0, "value of k2 must be 'k2-their'");

	succeed_if (elektraMergeGetConflicts (information) == 1, "must have 1 conflicts");

	keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
}

static void test_changed_same_key_our (void)
{
	printf ("test conflict with OUR\n");

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

	KeySet * result = elektraMerge (ksOurs, oursRoot, ksTheirs, theirsRoot, ksBase, baseRoot, root, MERGE_STRATEGY_OUR, information);

	succeed_if (result != NULL, "result must not be NULL");

	succeed_if (ksGetSize (result) == 2, "result must contain 2 keys");
	Key * k1 = ksLookupByName (result, "system:/test/k1", KDB_O_NONE);
	succeed_if (k1 != NULL, "system:/test/k1 must be included in result");
	succeed_if (strcmp ("k1", keyString (k1)) == 0, "value of k1 must be 'k1'");

	Key * k2 = ksLookupByName (result, "system:/test/k2", KDB_O_NONE);
	succeed_if (k2 != NULL, "system:/test/k2 must be included in result");
	succeed_if (strcmp ("k2-our", keyString (k2)) == 0, "value of k2 must be 'k2-our'");

	succeed_if (elektraMergeGetConflicts (information) == 1, "must have 1 conflicts");

	keyDel (baseRoot);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (root);
	keyDel (information);

	ksDel (ksBase);
	ksDel (ksOurs);
	ksDel (ksTheirs);
	ksDel (result);
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

	printf ("\ntest_merge RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
