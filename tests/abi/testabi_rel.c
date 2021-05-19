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

	Key * k1 = keyNew ("user:/valid", KEY_END);
	Key * k2 = keyNew ("user:/valid", KEY_END);

	succeed_if (keyCmp (0, 0) == 0, "all null pointers are same");

	//! [cmp null]
	succeed_if (keyCmp (0, 0) == 0, "all null pointers same");
	succeed_if (keyCmp (k1, 0) == 1, "null pointer is smaller");
	succeed_if (keyCmp (0, k2) == -1, "null pointer is smaller");
	//! [cmp null]

	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "");
	keySetName (k2, "");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "user:/");
	keySetName (k2, "user:/");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "system:/");
	keySetName (k2, "system:/");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "user:/");
	keySetName (k2, "system:/");
	succeed_if (keyCmp (k1, k2) < 0, "system is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "system is smaller");

	keySetName (k1, "user:/a");
	keySetName (k2, "user:/a");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");
	succeed_if (keyCmp (k2, k1) == 0, "should be same");

	keySetName (k1, "user:/a");
	keySetName (k2, "user:/b");
	succeed_if (keyCmp (k1, k2) < 0, "a is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "a is smaller");

	keySetName (k1, "user:/a/a");
	keySetName (k2, "user:/a-a");
	succeed_if (keyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "/ is smaller");

	char cmp[] = "user:/a-a";
	for (int i = 1; i < 256; ++i)
	{
		if (i == '/') continue;
		cmp[7] = i;
		// printf ("%i %s\n", i, cmp);
		keySetName (k1, "user:/a/a");
		keySetName (k2, cmp);
		succeed_if (keyCmp (k1, k2) < 0, "/ is smaller");
		succeed_if (keyCmp (k2, k1) > 0, "/ is smaller");
	}

	keySetName (k1, "user:/a");
	keySetName (k2, "user:/a/a");
	succeed_if (keyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "/ is smaller");

	keySetName (k1, "user:/a");
	keySetName (k2, "user:/a-a");
	succeed_if (keyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "/ is smaller");

	keySetName (k1, "user:/a");
	keySetName (k2, "user:/aa");
	succeed_if (keyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "/ is smaller");

	keySetName (k1, "user:/a");
	keySetName (k2, "user:/a-");
	succeed_if (keyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "/ is smaller");

	keySetName (k1, "user:/find_me");
	keySetName (k2, "user:/find_me/a");
	succeed_if (keyCmp (k1, k2) < 0, "find_me is smaller");
	succeed_if (keyCmp (k2, k1) > 0, "find_me is smaller");

	keySetName (k1, "user:user_a/b");
	keySetName (k2, "user:user_b/a");
	succeed_if (keyCmp (k1, k2) < 0, "should be same as owner is not considered");
	succeed_if (keyCmp (k2, k1) > 0, "should be same as owner is not considered");

	keyDel (k1);
	keyDel (k2);
}

static void test_directbelow (void)
{
	printf ("check if direct below\n");
	Key * k1 = keyNew ("/", KEY_END);
	Key * k2 = keyNew ("/", KEY_END);

	keySetName (k1, "user:/");
	keySetName (k2, "user:/a");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "system:/");
	keySetName (k2, "system:/a");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "user:/");
	keySetName (k2, "user:/longer_name");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "system:/");
	keySetName (k2, "system:/longer_name");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "user:/a");
	keySetName (k2, "user:/a/a");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "system:/a");
	keySetName (k2, "system:/a/a");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "system:/a\\/a");
	keySetName (k2, "system:/a\\/a/a");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "system:/a\\/a\\/a");
	keySetName (k2, "system:/a\\/a\\/a/b");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	keySetName (k1, "system:/a\\/a\\/a");
	keySetName (k2, "system:/a\\/a\\/a/b");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be direct below");


	keyDel (k1);
	keyDel (k2);
}

static void test_below (void)
{
	printf ("check if below\n");
	Key * k1 = keyNew ("/", KEY_END);
	Key * k2 = keyNew ("/", KEY_END);

	keySetName (k1, "user:/tests/simple");
	keySetName (k2, "user:/tests/simple/below");
	succeed_if (keyCmp (k1, k2) == 0 || keyIsBelow (k1, k2) == 1, "should be below");
	succeed_if (keyIsDirectlyBelow (k1, k2) == 1, "should be below");

	keySetName (k1, "user:/");
	keySetName (k2, "user:/a/a");
	succeed_if (keyIsBelow (k1, k2) == 1, "should be below");

	keySetName (k1, "system:/");
	keySetName (k2, "system:/a/a");
	succeed_if (keyIsBelow (k1, k2) == 1, "should be below");

	keySetName (k1, "user:/");
	keySetName (k2, "user:/longer_name/also_longer_name");
	succeed_if (keyIsBelow (k1, k2) == 1, "should be below");

	keySetName (k1, "system:/");
	keySetName (k2, "system:/longer_name/also_longer_name");
	succeed_if (keyIsBelow (k1, k2) == 1, "should be below");

	keySetName (k1, "user:/a");
	keySetName (k2, "user:/a/a/a/a/a/a");
	succeed_if (keyIsBelow (k1, k2) == 1, "should be below");

	keySetName (k1, "system:/a");
	keySetName (k2, "system:/a/a/a/a/a/a");
	succeed_if (keyIsBelow (k1, k2) == 1, "should be below");

	// TODO add test for cascading Keys

	keyDel (k1);
	keyDel (k2);
}

static void test_examples (void)
{
	printf ("check examples\n");
	Key * key = keyNew ("/", KEY_END);
	Key * check = keyNew ("/", KEY_END);

	keySetName (key, "user:/key/folder");
	keySetName (check, "user:/key/folder");
	succeed_if (keyCmp (key, check) == 0, "should be same");

	keySetName (key, "user:/key/folder");
	keySetName (check, "user:/key/folder/child");
	succeed_if (keyIsDirectlyBelow (key, check) == 1, "should be direct below");

	keySetName (key, "user:/key/folder");
	keySetName (check, "user:/key/folder/any/depth/deeper/grand-child");
	succeed_if (keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 1, "should be below (but not direct)");
	succeed_if (keyIsBelow (key, check) == 1, "should be below");
	succeed_if (keyCmp (key, check) == 0 || keyIsBelow (key, check) == 1, "should be the same or below");

	keySetName (key, "user:/key/folder");
	keySetName (check, "user:/notsame/folder");
	succeed_if (keyCmp (key, check) != 0 && keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 0, "key is not below");

	keySetName (key, "user:/key/folder");
	keySetName (check, "system:/notsame/folder");
	int has_no_rel = keyCmp (key, check) != 0 && keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 0;
	succeed_if (has_no_rel == 1, "not in the same namespace");

	keyDel (key);
	keyDel (check);
}

static void test_hierarchy (void)
{
	printf ("check hierarchy\n");
	Key * key = keyNew ("/", KEY_END);
	Key * check = keyNew ("/", KEY_END);

	keySetName (key, "user:/key/folder/key");
	keySetName (check, "user:/other/folder/key");
	succeed_if (keyCmp (key, check) != 0 && keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 0, "should be same");

	keySetName (key, "system:/key/folder/key");
	keySetName (check, "system:/other/folder/key");
	succeed_if (keyCmp (key, check) != 0 && keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 0, "should be same");

	keySetName (key, "user:/key/folder/key");
	keySetName (check, "system:/other/folder/key");

	int has_no_rel = keyCmp (key, check) != 0 && keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 0;
	succeed_if (has_no_rel == 1, "should be different (1)");

	keySetName (key, "system:/key/folder/key");
	keySetName (check, "user:/other/folder/key");

	has_no_rel = keyCmp (key, check) != 0 && keyIsDirectlyBelow (key, check) == 0 && keyIsBelow (key, check) == 0;
	succeed_if (has_no_rel == 1, "should be different (2)");
	keyDel (key);
	keyDel (check);
}

void test_keyCmpNsOrder (void)
{
	Key * cascadingKey = keyNew ("/key", KEY_END);
	Key * metaKey = keyNew ("meta:/key", KEY_END);
	Key * specKey = keyNew ("spec:/key", KEY_END);
	Key * procKey = keyNew ("proc:/key", KEY_END);
	Key * dirKey = keyNew ("dir:/key", KEY_END);
	Key * userKey = keyNew ("user:/key", KEY_END);
	Key * systemKey = keyNew ("system:/key", KEY_END);
	Key * defaultKey = keyNew ("default:/key", KEY_END);

	succeed_if (keyCmp (cascadingKey, cascadingKey) == 0, "cascading not equal to cascading");
	succeed_if (keyCmp (cascadingKey, metaKey) < 0, "cascading not smaller than meta");
	succeed_if (keyCmp (cascadingKey, specKey) < 0, "cascading not smaller than spec");
	succeed_if (keyCmp (cascadingKey, procKey) < 0, "cascading not smaller than proc");
	succeed_if (keyCmp (cascadingKey, dirKey) < 0, "cascading not smaller than dir");
	succeed_if (keyCmp (cascadingKey, userKey) < 0, "cascading not smaller than user");
	succeed_if (keyCmp (cascadingKey, systemKey) < 0, "cascading not smaller than system");
	succeed_if (keyCmp (cascadingKey, defaultKey) < 0, "cascading not smaller than default");

	succeed_if (keyCmp (metaKey, cascadingKey) > 0, "meta not greater than cascading");
	succeed_if (keyCmp (metaKey, metaKey) == 0, "meta not equal to meta");
	succeed_if (keyCmp (metaKey, specKey) < 0, "meta not smaller than spec");
	succeed_if (keyCmp (metaKey, procKey) < 0, "meta not smaller than proc");
	succeed_if (keyCmp (metaKey, dirKey) < 0, "meta not smaller than dir");
	succeed_if (keyCmp (metaKey, userKey) < 0, "meta not smaller than user");
	succeed_if (keyCmp (metaKey, systemKey) < 0, "meta not smaller than system");
	succeed_if (keyCmp (metaKey, defaultKey) < 0, "meta not smaller than default");

	succeed_if (keyCmp (specKey, cascadingKey) > 0, "spec not greater than cascading");
	succeed_if (keyCmp (specKey, metaKey) > 0, "spec not greater than meta");
	succeed_if (keyCmp (specKey, specKey) == 0, "spec not equal to spec");
	succeed_if (keyCmp (specKey, procKey) < 0, "spec not smaller than proc");
	succeed_if (keyCmp (specKey, dirKey) < 0, "spec not smaller than dir");
	succeed_if (keyCmp (specKey, userKey) < 0, "spec not smaller than user");
	succeed_if (keyCmp (specKey, systemKey) < 0, "spec not smaller than system");
	succeed_if (keyCmp (specKey, defaultKey) < 0, "spec not smaller than default");

	succeed_if (keyCmp (procKey, cascadingKey) > 0, "proc not greater than cascading");
	succeed_if (keyCmp (procKey, metaKey) > 0, "proc not greater than meta");
	succeed_if (keyCmp (procKey, specKey) > 0, "proc not greater than spec");
	succeed_if (keyCmp (procKey, procKey) == 0, "proc not equal to proc");
	succeed_if (keyCmp (procKey, dirKey) < 0, "proc not smaller than dir");
	succeed_if (keyCmp (procKey, userKey) < 0, "proc not smaller than user");
	succeed_if (keyCmp (procKey, systemKey) < 0, "proc not smaller than system");
	succeed_if (keyCmp (procKey, defaultKey) < 0, "proc not smaller than default");

	succeed_if (keyCmp (dirKey, cascadingKey) > 0, "dir not greater than cascading");
	succeed_if (keyCmp (dirKey, metaKey) > 0, "dir not greater than meta");
	succeed_if (keyCmp (dirKey, specKey) > 0, "dir not greater than spec");
	succeed_if (keyCmp (dirKey, procKey) > 0, "dir not grater than proc");
	succeed_if (keyCmp (dirKey, dirKey) == 0, "dir not equal to dir");
	succeed_if (keyCmp (dirKey, userKey) < 0, "dir not smaller than user");
	succeed_if (keyCmp (dirKey, systemKey) < 0, "dir not smaller than system");
	succeed_if (keyCmp (dirKey, defaultKey) < 0, "dir not smaller than default");

	succeed_if (keyCmp (userKey, cascadingKey) > 0, "user not greater than cascading");
	succeed_if (keyCmp (userKey, metaKey) > 0, "user not greater than meta");
	succeed_if (keyCmp (userKey, specKey) > 0, "user not greater than spec");
	succeed_if (keyCmp (userKey, procKey) > 0, "user not greater than proc");
	succeed_if (keyCmp (userKey, dirKey) > 0, "user not greater than dir");
	succeed_if (keyCmp (userKey, userKey) == 0, "user not eqaul user");
	succeed_if (keyCmp (userKey, systemKey) < 0, "user not smaller than system");
	succeed_if (keyCmp (userKey, defaultKey) < 0, "user not smaller than default");

	succeed_if (keyCmp (systemKey, cascadingKey) > 0, "system not greater than cascading");
	succeed_if (keyCmp (systemKey, metaKey) > 0, "system not greater than meta");
	succeed_if (keyCmp (systemKey, specKey) > 0, "system not greater than spec");
	succeed_if (keyCmp (systemKey, procKey) > 0, "system not greater than proc");
	succeed_if (keyCmp (systemKey, dirKey) > 0, "system not greater than dir");
	succeed_if (keyCmp (systemKey, userKey) > 0, "system not greater than user");
	succeed_if (keyCmp (systemKey, systemKey) == 0, "system not equal to system");
	succeed_if (keyCmp (systemKey, defaultKey) < 0, "system not smaller than default");

	succeed_if (keyCmp (defaultKey, cascadingKey) > 0, "default not greater than cascading");
	succeed_if (keyCmp (defaultKey, metaKey) > 0, "default not greater than meta");
	succeed_if (keyCmp (defaultKey, specKey) > 0, "default not greater than spec");
	succeed_if (keyCmp (defaultKey, procKey) > 0, "default not greater than proc");
	succeed_if (keyCmp (defaultKey, dirKey) > 0, "default not greater than dir");
	succeed_if (keyCmp (defaultKey, userKey) > 0, "default not greater than user");
	succeed_if (keyCmp (defaultKey, systemKey) > 0, "default not greater than system");
	succeed_if (keyCmp (defaultKey, defaultKey) == 0, "default not equal to default");

	keyDel (cascadingKey);
	keyDel (metaKey);
	keyDel (specKey);
	keyDel (procKey);
	keyDel (dirKey);
	keyDel (userKey);
	keyDel (systemKey);
	keyDel (defaultKey);
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
	test_keyCmpNsOrder ();

	printf ("\ntest_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
