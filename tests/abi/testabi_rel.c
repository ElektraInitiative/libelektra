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

	ElektraKey * k1 = elektraKeyNew ("user:/valid", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("user:/valid", ELEKTRA_KEY_END);

	succeed_if (elektraKeyCmp (0, 0) == 0, "all null pointers are same");

	//! [cmp null]
	succeed_if (elektraKeyCmp (0, 0) == 0, "all null pointers same");
	succeed_if (elektraKeyCmp (k1, 0) == 1, "null pointer is smaller");
	succeed_if (elektraKeyCmp (0, k2) == -1, "null pointer is smaller");
	//! [cmp null]

	succeed_if (elektraKeyCmp (k1, k2) == 0, "should be same");

	elektraKeySetName (k1, "");
	elektraKeySetName (k2, "");
	succeed_if (elektraKeyCmp (k1, k2) == 0, "should be same");

	succeed_if (elektraKeySetName (k1, "user:/") == 7, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/") == 7, "should be a valid name");
	succeed_if (elektraKeyCmp (k1, k2) == 0, "should be same");

	succeed_if (elektraKeySetName (k1, "system:/") == 9, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "system:/") == 9, "should be a valid name");
	succeed_if (elektraKeyCmp (k1, k2) == 0, "should be same");

	succeed_if (elektraKeySetName (k1, "user:/") == 7, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "system:/") == 9, "should be a valid name");
	succeed_if (elektraKeyCmp (k1, k2) < 0, "system is smaller");
	succeed_if (elektraKeyCmp (k2, k1) > 0, "system is smaller");

	succeed_if (elektraKeySetName (k1, "user:/a") == 8, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/a") == 8, "should be a valid name");
	succeed_if (elektraKeyCmp (k1, k2) == 0, "should be same");
	succeed_if (elektraKeyCmp (k2, k1) == 0, "should be same");

	succeed_if (elektraKeySetName (k1, "user:/a") == 8, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/b") == 8, "should be a valid name");
	succeed_if (elektraKeyCmp (k1, k2) < 0, "a is smaller");
	succeed_if (elektraKeyCmp (k2, k1) > 0, "a is smaller");

	succeed_if (elektraKeySetName (k1, "user:/a/a") == 10, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/a-a") == 10, "should be a valid name");
	succeed_if (elektraKeyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (elektraKeyCmp (k2, k1) > 0, "/ is smaller");

	char cmp[] = "user:/a-a";
	for (int i = 1; i < 256; ++i)
	{
		if (i == '/') continue;
		cmp[7] = i;
		// printf ("%i %s\n", i, cmp);
		succeed_if (elektraKeySetName (k1, "user:/a/a") == 10, "should be a valid name");
		elektraKeySetName (k2, cmp);
		succeed_if (elektraKeyCmp (k1, k2) < 0, "/ is smaller");
		succeed_if (elektraKeyCmp (k2, k1) > 0, "/ is smaller");
	}

	succeed_if (elektraKeySetName (k1, "user:/a") == 8, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/a/a") == 10, "should be a valid name");
	succeed_if (elektraKeyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (elektraKeyCmp (k2, k1) > 0, "/ is smaller");

	succeed_if (elektraKeySetName (k1, "user:/a") == 8, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/a-a") == 10, "should be a valid name");
	succeed_if (elektraKeyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (elektraKeyCmp (k2, k1) > 0, "/ is smaller");

	succeed_if (elektraKeySetName (k1, "user:/a") == 8, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/aa") == 9, "should be a valid name");
	succeed_if (elektraKeyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (elektraKeyCmp (k2, k1) > 0, "/ is smaller");

	succeed_if (elektraKeySetName (k1, "user:/a") == 8, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/a-") == 9, "should be a valid name");
	succeed_if (elektraKeyCmp (k1, k2) < 0, "/ is smaller");
	succeed_if (elektraKeyCmp (k2, k1) > 0, "/ is smaller");

	succeed_if (elektraKeySetName (k1, "user:/find_me") == 14, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/find_me/a") == 16, "should be a valid name");
	succeed_if (elektraKeyCmp (k1, k2) < 0, "find_me is smaller");
	succeed_if (elektraKeyCmp (k2, k1) > 0, "find_me is smaller");

	elektraKeyDel (k1);
	elektraKeyDel (k2);
}

static void test_directbelow (void)
{
	printf ("check if direct below\n");
	ElektraKey * k1 = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("/", ELEKTRA_KEY_END);

	succeed_if (elektraKeySetName (k1, "user:/") == 7, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/a") == 8, "should be a valid name");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	succeed_if (elektraKeySetName (k1, "system:/") == 9, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "system:/a") == 10, "should be a valid name");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	succeed_if (elektraKeySetName (k1, "user:/") == 7, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/longer_name") == 18, "should be a valid name");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	succeed_if (elektraKeySetName (k1, "system:/") == 9, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "system:/longer_name") == 20, "should be a valid name");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	succeed_if (elektraKeySetName (k1, "user:/a") == 8, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/a/a") == 10, "should be a valid name");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	succeed_if (elektraKeySetName (k1, "system:/a") == 10, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "system:/a/a") == 12, "should be a valid name");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	succeed_if (elektraKeySetName (k1, "system:/a\\/a") == 13, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "system:/a\\/a/a") == 15, "should be a valid name");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	succeed_if (elektraKeySetName (k1, "system:/a\\/a\\/a") == 16, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "system:/a\\/a\\/a/b") == 18, "should be a valid name");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "should be direct below");

	succeed_if (elektraKeySetName (k1, "system:/a\\/a\\/a") == 16, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "system:/a\\/a\\/a/b") == 18, "should be a valid name");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "should be direct below");


	elektraKeyDel (k1);
	elektraKeyDel (k2);
}

static void test_below (void)
{
	printf ("check if below\n");
	ElektraKey * k1 = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("/", ELEKTRA_KEY_END);

	succeed_if (elektraKeySetName (k1, "user:/tests/simple") == 19, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/tests/simple/below") == 25, "should be a valid name");
	succeed_if (elektraKeyCmp (k1, k2) == 0 || elektraKeyIsBelow (k1, k2) == 1, "should be below");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "should be below");

	succeed_if (elektraKeySetName (k1, "user:/") == 7, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/a/a") == 10, "should be a valid name");
	succeed_if (elektraKeyIsBelow (k1, k2) == 1, "should be below");

	succeed_if (elektraKeySetName (k1, "system:/") == 9, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "system:/a/a") == 12, "should be a valid name");
	succeed_if (elektraKeyIsBelow (k1, k2) == 1, "should be below");

	succeed_if (elektraKeySetName (k1, "user:/") == 7, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/longer_name/also_longer_name") == 35, "should be a valid name");
	succeed_if (elektraKeyIsBelow (k1, k2) == 1, "should be below");

	succeed_if (elektraKeySetName (k1, "system:/") == 9, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "system:/longer_name/also_longer_name") == 37, "should be a valid name");
	succeed_if (elektraKeyIsBelow (k1, k2) == 1, "should be below");

	succeed_if (elektraKeySetName (k1, "user:/a") == 8, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/a/a/a/a/a/a") == 18, "should be a valid name");
	succeed_if (elektraKeyIsBelow (k1, k2) == 1, "should be below");

	succeed_if (elektraKeySetName (k1, "system:/a") == 10, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "system:/a/a/a/a/a/a") == 20, "should be a valid name");
	succeed_if (elektraKeyIsBelow (k1, k2) == 1, "should be below");

	succeed_if (elektraKeySetName (k1, "/") == 2, "should be a valid name");
	succeed_if (elektraKeySetName (k2, "user:/something") == 16, "should be a valid name");
	succeed_if (elektraKeyIsBelow (k1, k2) == 1, "should be below");

	elektraKeyDel (k1);
	elektraKeyDel (k2);
}

static void test_examples (void)
{
	printf ("check examples\n");
	ElektraKey * key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * check = elektraKeyNew ("/", ELEKTRA_KEY_END);

	succeed_if (elektraKeySetName (key, "user:/key/folder") == 17, "should be a valid name");
	succeed_if (elektraKeySetName (check, "user:/key/folder") == 17, "should be a valid name");
	succeed_if (elektraKeyCmp (key, check) == 0, "should be same");

	succeed_if (elektraKeySetName (key, "user:/key/folder") == 17, "should be a valid name");
	succeed_if (elektraKeySetName (check, "user:/key/folder/child") == 23, "should be a valid name");
	succeed_if (elektraKeyIsDirectlyBelow (key, check) == 1, "should be direct below");

	succeed_if (elektraKeySetName (key, "user:/key/folder") == 17, "should be a valid name");
	succeed_if (elektraKeySetName (check, "user:/key/folder/any/depth/deeper/grand-child") == 46, "should be a valid name");
	succeed_if (elektraKeyIsDirectlyBelow (key, check) == 0 && elektraKeyIsBelow (key, check) == 1, "should be below (but not direct)");
	succeed_if (elektraKeyIsBelow (key, check) == 1, "should be below");
	succeed_if (elektraKeyCmp (key, check) == 0 || elektraKeyIsBelow (key, check) == 1, "should be the same or below");

	succeed_if (elektraKeySetName (key, "user:/key/folder") == 17, "should be a valid name");
	succeed_if (elektraKeySetName (check, "user:/notsame/folder") == 21, "should be a valid name");
	succeed_if (elektraKeyCmp (key, check) != 0 && elektraKeyIsDirectlyBelow (key, check) == 0 && elektraKeyIsBelow (key, check) == 0, "key is not below");

	succeed_if (elektraKeySetName (key, "user:/key/folder") == 17, "should be a valid name");
	succeed_if (elektraKeySetName (check, "system:/notsame/folder") == 23, "should be a valid name");
	int has_no_rel = elektraKeyCmp (key, check) != 0 && elektraKeyIsDirectlyBelow (key, check) == 0 && elektraKeyIsBelow (key, check) == 0;
	succeed_if (has_no_rel == 1, "not in the same namespace");

	elektraKeyDel (key);
	elektraKeyDel (check);
}

static void test_hierarchy (void)
{
	printf ("check hierarchy\n");
	ElektraKey * key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * check = elektraKeyNew ("/", ELEKTRA_KEY_END);

	succeed_if (elektraKeySetName (key, "user:/key/folder/key") == 21, "should be a valid name");
	succeed_if (elektraKeySetName (check, "user:/other/folder/key") == 23, "should be a valid name");
	succeed_if (elektraKeyCmp (key, check) != 0 && elektraKeyIsDirectlyBelow (key, check) == 0 && elektraKeyIsBelow (key, check) == 0, "should be same");

	succeed_if (elektraKeySetName (key, "system:/key/folder/key") == 23, "should be a valid name");
	succeed_if (elektraKeySetName (check, "system:/other/folder/key") == 25, "should be a valid name");
	succeed_if (elektraKeyCmp (key, check) != 0 && elektraKeyIsDirectlyBelow (key, check) == 0 && elektraKeyIsBelow (key, check) == 0, "should be same");

	succeed_if (elektraKeySetName (key, "user:/key/folder/key") == 21, "should be a valid name");
	succeed_if (elektraKeySetName (check, "system:/other/folder/key") == 25, "should be a valid name");

	int has_no_rel = elektraKeyCmp (key, check) != 0 && elektraKeyIsDirectlyBelow (key, check) == 0 && elektraKeyIsBelow (key, check) == 0;
	succeed_if (has_no_rel == 1, "should be different (1)");

	succeed_if (elektraKeySetName (key, "system:/key/folder/key") == 23, "should be a valid name");
	succeed_if (elektraKeySetName (check, "user:/other/folder/key") == 23, "should be a valid name");

	has_no_rel = elektraKeyCmp (key, check) != 0 && elektraKeyIsDirectlyBelow (key, check) == 0 && elektraKeyIsBelow (key, check) == 0;
	succeed_if (has_no_rel == 1, "should be different (2)");
	elektraKeyDel (key);
	elektraKeyDel (check);
}

void test_keyCmpNsOrder (void)
{
	ElektraKey * cascadingKey = elektraKeyNew ("/key", ELEKTRA_KEY_END);
	ElektraKey * metaKey = elektraKeyNew ("meta:/key", ELEKTRA_KEY_END);
	ElektraKey * specKey = elektraKeyNew ("spec:/key", ELEKTRA_KEY_END);
	ElektraKey * procKey = elektraKeyNew ("proc:/key", ELEKTRA_KEY_END);
	ElektraKey * dirKey = elektraKeyNew ("dir:/key", ELEKTRA_KEY_END);
	ElektraKey * userKey = elektraKeyNew ("user:/key", ELEKTRA_KEY_END);
	ElektraKey * systemKey = elektraKeyNew ("system:/key", ELEKTRA_KEY_END);
	ElektraKey * defaultKey = elektraKeyNew ("default:/key", ELEKTRA_KEY_END);

	succeed_if (elektraKeyCmp (cascadingKey, cascadingKey) == 0, "cascading not equal to cascading");
	succeed_if (elektraKeyCmp (cascadingKey, metaKey) < 0, "cascading not smaller than meta");
	succeed_if (elektraKeyCmp (cascadingKey, specKey) < 0, "cascading not smaller than spec");
	succeed_if (elektraKeyCmp (cascadingKey, procKey) < 0, "cascading not smaller than proc");
	succeed_if (elektraKeyCmp (cascadingKey, dirKey) < 0, "cascading not smaller than dir");
	succeed_if (elektraKeyCmp (cascadingKey, userKey) < 0, "cascading not smaller than user");
	succeed_if (elektraKeyCmp (cascadingKey, systemKey) < 0, "cascading not smaller than system");
	succeed_if (elektraKeyCmp (cascadingKey, defaultKey) < 0, "cascading not smaller than default");

	succeed_if (elektraKeyCmp (metaKey, cascadingKey) > 0, "meta not greater than cascading");
	succeed_if (elektraKeyCmp (metaKey, metaKey) == 0, "meta not equal to meta");
	succeed_if (elektraKeyCmp (metaKey, specKey) < 0, "meta not smaller than spec");
	succeed_if (elektraKeyCmp (metaKey, procKey) < 0, "meta not smaller than proc");
	succeed_if (elektraKeyCmp (metaKey, dirKey) < 0, "meta not smaller than dir");
	succeed_if (elektraKeyCmp (metaKey, userKey) < 0, "meta not smaller than user");
	succeed_if (elektraKeyCmp (metaKey, systemKey) < 0, "meta not smaller than system");
	succeed_if (elektraKeyCmp (metaKey, defaultKey) < 0, "meta not smaller than default");

	succeed_if (elektraKeyCmp (specKey, cascadingKey) > 0, "spec not greater than cascading");
	succeed_if (elektraKeyCmp (specKey, metaKey) > 0, "spec not greater than meta");
	succeed_if (elektraKeyCmp (specKey, specKey) == 0, "spec not equal to spec");
	succeed_if (elektraKeyCmp (specKey, procKey) < 0, "spec not smaller than proc");
	succeed_if (elektraKeyCmp (specKey, dirKey) < 0, "spec not smaller than dir");
	succeed_if (elektraKeyCmp (specKey, userKey) < 0, "spec not smaller than user");
	succeed_if (elektraKeyCmp (specKey, systemKey) < 0, "spec not smaller than system");
	succeed_if (elektraKeyCmp (specKey, defaultKey) < 0, "spec not smaller than default");

	succeed_if (elektraKeyCmp (procKey, cascadingKey) > 0, "proc not greater than cascading");
	succeed_if (elektraKeyCmp (procKey, metaKey) > 0, "proc not greater than meta");
	succeed_if (elektraKeyCmp (procKey, specKey) > 0, "proc not greater than spec");
	succeed_if (elektraKeyCmp (procKey, procKey) == 0, "proc not equal to proc");
	succeed_if (elektraKeyCmp (procKey, dirKey) < 0, "proc not smaller than dir");
	succeed_if (elektraKeyCmp (procKey, userKey) < 0, "proc not smaller than user");
	succeed_if (elektraKeyCmp (procKey, systemKey) < 0, "proc not smaller than system");
	succeed_if (elektraKeyCmp (procKey, defaultKey) < 0, "proc not smaller than default");

	succeed_if (elektraKeyCmp (dirKey, cascadingKey) > 0, "dir not greater than cascading");
	succeed_if (elektraKeyCmp (dirKey, metaKey) > 0, "dir not greater than meta");
	succeed_if (elektraKeyCmp (dirKey, specKey) > 0, "dir not greater than spec");
	succeed_if (elektraKeyCmp (dirKey, procKey) > 0, "dir not grater than proc");
	succeed_if (elektraKeyCmp (dirKey, dirKey) == 0, "dir not equal to dir");
	succeed_if (elektraKeyCmp (dirKey, userKey) < 0, "dir not smaller than user");
	succeed_if (elektraKeyCmp (dirKey, systemKey) < 0, "dir not smaller than system");
	succeed_if (elektraKeyCmp (dirKey, defaultKey) < 0, "dir not smaller than default");

	succeed_if (elektraKeyCmp (userKey, cascadingKey) > 0, "user not greater than cascading");
	succeed_if (elektraKeyCmp (userKey, metaKey) > 0, "user not greater than meta");
	succeed_if (elektraKeyCmp (userKey, specKey) > 0, "user not greater than spec");
	succeed_if (elektraKeyCmp (userKey, procKey) > 0, "user not greater than proc");
	succeed_if (elektraKeyCmp (userKey, dirKey) > 0, "user not greater than dir");
	succeed_if (elektraKeyCmp (userKey, userKey) == 0, "user not eqaul user");
	succeed_if (elektraKeyCmp (userKey, systemKey) < 0, "user not smaller than system");
	succeed_if (elektraKeyCmp (userKey, defaultKey) < 0, "user not smaller than default");

	succeed_if (elektraKeyCmp (systemKey, cascadingKey) > 0, "system not greater than cascading");
	succeed_if (elektraKeyCmp (systemKey, metaKey) > 0, "system not greater than meta");
	succeed_if (elektraKeyCmp (systemKey, specKey) > 0, "system not greater than spec");
	succeed_if (elektraKeyCmp (systemKey, procKey) > 0, "system not greater than proc");
	succeed_if (elektraKeyCmp (systemKey, dirKey) > 0, "system not greater than dir");
	succeed_if (elektraKeyCmp (systemKey, userKey) > 0, "system not greater than user");
	succeed_if (elektraKeyCmp (systemKey, systemKey) == 0, "system not equal to system");
	succeed_if (elektraKeyCmp (systemKey, defaultKey) < 0, "system not smaller than default");

	succeed_if (elektraKeyCmp (defaultKey, cascadingKey) > 0, "default not greater than cascading");
	succeed_if (elektraKeyCmp (defaultKey, metaKey) > 0, "default not greater than meta");
	succeed_if (elektraKeyCmp (defaultKey, specKey) > 0, "default not greater than spec");
	succeed_if (elektraKeyCmp (defaultKey, procKey) > 0, "default not greater than proc");
	succeed_if (elektraKeyCmp (defaultKey, dirKey) > 0, "default not greater than dir");
	succeed_if (elektraKeyCmp (defaultKey, userKey) > 0, "default not greater than user");
	succeed_if (elektraKeyCmp (defaultKey, systemKey) > 0, "default not greater than system");
	succeed_if (elektraKeyCmp (defaultKey, defaultKey) == 0, "default not equal to default");

	elektraKeyDel (cascadingKey);
	elektraKeyDel (metaKey);
	elektraKeyDel (specKey);
	elektraKeyDel (procKey);
	elektraKeyDel (dirKey);
	elektraKeyDel (userKey);
	elektraKeyDel (systemKey);
	elektraKeyDel (defaultKey);
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
