/**
 * @file
 *
 * @brief Relation between keys.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests.h>


static void test_keyCmp (void)
{
	printf ("check keyCmp\n");

	Key * k1 = keyNew ("user/valid", KEY_END);
	Key * k2 = keyNew ("user/valid", KEY_END);

	Key * nk1 = keyNew (0);
	Key * nk2 = keyNew (0);

	succeed_if (keyCmp (0, 0) == 0, "all null pointers are same");
	succeed_if (keyCmp (nk1, 0) == 1, "null pointer is smaller");
	succeed_if (keyCmp (0, nk2) == -1, "null pointer is smaller");

	//! [cmp null]
	succeed_if (keyCmp (0, 0) == 0, "all null pointers same");
	succeed_if (keyCmp (k1, 0) == 1, "null pointer is smaller");
	succeed_if (keyCmp (0, k2) == -1, "null pointer is smaller");
	//! [cmp null]

	succeed_if (keyCmp (nk1, nk1) == 0, "all null keys are same");
	succeed_if (keyCmp (k1, nk1) == 1, "null keys are smaller");
	succeed_if (keyCmp (nk1, k2) == -1, "null keys are smaller");

	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "");
	keySetName (k2, "");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "user");
	keySetName (k2, "user");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "system");
	keySetName (k2, "system");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "system");
	keySetName (k2, "user");
	succeed_if (keyCmp (k1, k2) < 0, "system is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "system is smaller");

	keySetName (k1, "user/a");
	keySetName (k2, "user/a");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");
	succeed_if (keyCmp (k2, k1) == 0, "should be same");

	keySetName (k1, "user/a");
	keySetName (k2, "user/b");
	succeed_if (keyCmp (k1, k2) < 0, "a is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "a is smaller");

	keySetName (k1, "user/a/a");
	keySetName (k2, "user/a-a");
	succeed_if (keyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "/ is smaller");

	char cmp[] = "user/a-a";
	for (int i = 1; i < 256; ++i)
	{
		if (i == '/') continue;
		cmp[6] = i;
		// printf ("%i %s\n", i, cmp);
		keySetName (k1, "user/a/a");
		keySetName (k2, cmp);
		succeed_if (keyCmp (k1, k2) < 0, "/ is smaller");
		succeed_if (keyCmp (k2, k1) > 0, "/ is smaller");
	}

	keySetName (k1, "user/a");
	keySetName (k2, "user/a/a");
	succeed_if (keyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "/ is smaller");

	keySetName (k1, "user/a");
	keySetName (k2, "user/a-a");
	succeed_if (keyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "/ is smaller");

	keySetName (k1, "user/a");
	keySetName (k2, "user/aa");
	succeed_if (keyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "/ is smaller");

	keySetName (k1, "user/a");
	keySetName (k2, "user/a-");
	succeed_if (keyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "/ is smaller");

	keySetName (k1, "user/find_me");
	keySetName (k2, "user/find_me/a");
	succeed_if (keyCmp (k1, k2) < 0, "find_me is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "find_me is smaller");

	keyDel (nk1);
	keyDel (nk2);

	keyDel (k1);
	keyDel (k2);
}

static void test_directbelow (void)
{
	printf ("check if direct below\n");
	Key * k1 = keyNew (0);
	Key * k2 = keyNew (0);

	keySetName (k1, "user");
	keySetName (k2, "user/a");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "system");
	keySetName (k2, "system/a");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "user");
	keySetName (k2, "user/longer_name");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "system");
	keySetName (k2, "system/longer_name");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "user/a");
	keySetName (k2, "user/a/a");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "system/a");
	keySetName (k2, "system/a/a");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "system/a\\/a");
	keySetName (k2, "system/a\\/a/a");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "system/a\\/a\\/a");
	keySetName (k2, "system/a\\/a\\/a/b");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "system/a\\/a\\/a");
	keySetName (k2, "system/a\\/a\\/a/b");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");


	keyDel (k1);
	keyDel (k2);
}

static void test_below (void)
{
	printf ("check if below\n");
	Key * k1 = keyNew (0);
	Key * k2 = keyNew (0);

	keySetName (k1, "user/tests/simple");
	keySetName (k2, "user/tests/simple/below");
	succeed_if (keyCmp (k1, k2) == 0 || keyIsBelow (k1, k2) == 1, "should be below");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be below");

	keySetName (k1, "user");
	keySetName (k2, "user/a/a");
	succeed_if (keyIsBelow (k1, k2) == 1, "should be below");

	keySetName (k1, "system");
	keySetName (k2, "system/a/a");
	succeed_if (keyIsBelow (k1, k2) == 1, "should be below");

	keySetName (k1, "user");
	keySetName (k2, "user/longer_name/also_longer_name");
	succeed_if (keyIsBelow (k1, k2) == 1, "should be below");

	keySetName (k1, "system");
	keySetName (k2, "system/longer_name/also_longer_name");
	succeed_if (keyIsBelow (k1, k2) == 1, "should be below");

	keySetName (k1, "user/a");
	keySetName (k2, "user/a/a/a/a/a/a");
	succeed_if (keyIsBelow (k1, k2) == 1, "should be below");

	keySetName (k1, "system/a");
	keySetName (k2, "system/a/a/a/a/a/a");
	succeed_if (keyIsBelow (k1, k2) == 1, "should be below");


	keyDel (k1);
	keyDel (k2);
}

static void test_examples (void)
{
	printf ("check examples\n");
	Key * key = keyNew (0);
	Key * check = keyNew (0);

	keySetName (key, "user/key/folder");
	keySetName (check, "user/key/folder");
	succeed_if (keyCmp (key, check) == 0, "should be same");

	keySetName (key, "user/key/folder");
	keySetName (check, "user/key/folder/child");
	succeed_if (keyIsDirectlyBelow (key, check) == 1, "should be direct below");

	keySetName (key, "user/key/folder");
	keySetName (check, "user/key/folder/any/depth/deeper/grand-child");
	succeed_if (keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 1, "should be below (but not direct)");
	succeed_if (keyIsBelow (key, check) == 1, "should be below");
	succeed_if (keyCmp (key, check) == 0 || keyIsBelow (key, check) == 1, "should be the same or below");

	keySetName (key, "user/key/folder");
	keySetName (check, "user/notsame/folder");
	succeed_if (keyCmp (key, check) != 0 && keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 0, "key is not below");

	keySetName (key, "user/key/folder");
	keySetName (check, "system/notsame/folder");
	int has_no_rel = keyCmp (key, check) != 0 && keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 0;
	succeed_if (has_no_rel == 1, "not in the same namespace");

	keyDel (key);
	keyDel (check);
}

static void test_hierarchy (void)
{
	printf ("check hierarchy\n");
	Key * key = keyNew (0);
	Key * check = keyNew (0);

	keySetName (key, "user/key/folder/key");
	keySetName (check, "user/other/folder/key");
	succeed_if (keyCmp (key, check) != 0 && keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 0, "should be same");

	keySetName (key, "system/key/folder/key");
	keySetName (check, "system/other/folder/key");
	succeed_if (keyCmp (key, check) != 0 && keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 0, "should be same");

	keySetName (key, "user/key/folder/key");
	keySetName (check, "system/other/folder/key");

	int has_no_rel = keyCmp (key, check) != 0 && keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 0;
	succeed_if (has_no_rel == 1, "should be different (1)");

	keySetName (key, "system/key/folder/key");
	keySetName (check, "user/other/folder/key");

	has_no_rel = keyCmp (key, check) != 0 && keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 0;
	succeed_if (has_no_rel == 1, "should be different (2)");
	keyDel (key);
	keyDel (check);
}

int main (int argc, char ** argv)
{
	printf ("KEY RELATION TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_keyCmp ();
	test_directbelow ();
	test_below ();
	test_examples ();
	test_hierarchy ();

	printf ("\ntest_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
