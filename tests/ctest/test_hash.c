/**
 * @file
 *
 * @brief Tests for src/libs/ease/hash.c.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "../../src/libs/ease/hash.c"
#include "../../src/libs/ease/sha-256.c"
#include <tests_internal.h>

/**
 * Test whether token calculation for KeySet is correct.
 *
 * Expected results calculated via GNU coreutils' sha256sum (GNU coreutils) 8.30.
 */
static void test_keySet (void) {
	char hash_string[65];
	KeySet * ks = ksNew (3,
		       keyNew ("/sw/application/myapp/#0/current", KEY_END),
		       KS_END);
	Key * parentKey = keyNew("/sw/application/myapp/#0/current", KEY_END);

	calculateSpecificationToken (hash_string, ks, parentKey);

	const char * expected = "495c901c07beb0aedd636a4d20390f503cb5a4f5af2f69d32995804059867403";
	succeed_if_fmt (strcmp(hash_string, expected) == 0, "Calculated token %s did not match expected result %s.", hash_string, expected);

	keyDel(parentKey);
	ksDel(ks);
}


/**
 * Test whether token calculation for KeySet ignores keys not equal or below parentKey.
 *
 * Expected results calculated via GNU coreutils' sha256sum (GNU coreutils) 8.30.
 */
static void test_onlyKeysBelowParentKey (void) {
	KeySet * ksOnlyWithKeysFromMyApp =
		ksNew (3,
		       keyNew ("/sw/application/myapp/#0/current", KEY_META, "mountpoint", "test.ecf", KEY_END),
		       keyNew ("/sw/application/myapp/#0/current/mykey", KEY_META, "default", "1", KEY_END),
		       keyNew ("/sw/application/myapp/#0/current/myotherkey", KEY_META, "opt/arg", "required", KEY_END),
		       KS_END);
	KeySet * ksWithKeysFromTwoApps = ksDup(ksOnlyWithKeysFromMyApp);
	ksAppendKey (ksWithKeysFromTwoApps, keyNew ("/sw/application/myotherapp/#0/current/somekey", KEY_META, "opt/arg", "required", KEY_END));
	ksAppendKey (ksOnlyWithKeysFromMyApp, keyNew ("/sw/application/myotherapp/#0/current/someotherkey", KEY_META, "opt/arg", "required", KEY_END));

	Key * parentKeyForMyApp = keyNew("/sw/application/myapp/#0/current", KEY_END);

	char hash_ksWithKeysFroMyApp[65];
	char hash_ksWithKeysFromTwoApps[65];
	calculateSpecificationToken (hash_ksWithKeysFroMyApp, ksOnlyWithKeysFromMyApp, parentKeyForMyApp);
	calculateSpecificationToken (hash_ksWithKeysFromTwoApps, ksWithKeysFromTwoApps, parentKeyForMyApp);

	succeed_if (strcmp(hash_ksWithKeysFroMyApp, hash_ksWithKeysFromTwoApps) == 0, "Token calculation did not properly cut out irrelevant keys! The hash for two different KeySets that have the same set of Keys below the parentKey should be the same, but it was different!");

	keyDel(parentKeyForMyApp);
	ksDel(ksOnlyWithKeysFromMyApp);
	ksDel(ksWithKeysFromTwoApps);
}

int main (int argc, char ** argv)
{
	printf ("HASH    TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_keySet();
	test_onlyKeysBelowParentKey();

	printf ("\ntest_hash RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
