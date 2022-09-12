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
static void test_keySet (void)
{
	char hash_string[65];
	ElektraKeyset * ks = elektraKeysetNew (3, elektraKeyNew ("/sw/application/myapp/#0/current", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKey * parentKey = elektraKeyNew ("/sw/application/myapp/#0/current", ELEKTRA_KEY_END);

	calculateSpecificationToken (hash_string, ks, parentKey);

	const char * expected = "495c901c07beb0aedd636a4d20390f503cb5a4f5af2f69d32995804059867403";
	succeed_if_fmt (strcmp (hash_string, expected) == 0, "Calculated token %s did not match expected result %s.", hash_string,
			expected);

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
}


/**
 * Test whether token calculation for KeySet ignores keys not equal or below parentKey.
 *
 * Expected results calculated via GNU coreutils' sha256sum (GNU coreutils) 8.30.
 */
static void test_onlyKeysBelowParentKey (void)
{
	ElektraKeyset * ksOnlyWithKeysFromMyApp =
		elektraKeysetNew (3, elektraKeyNew ("/sw/application/myapp/#0/current", ELEKTRA_KEY_META, "mountpoint", "test.ecf", ELEKTRA_KEY_END),
		       elektraKeyNew ("/sw/application/myapp/#0/current/mykey", ELEKTRA_KEY_META, "default", "1", ELEKTRA_KEY_END),
		       elektraKeyNew ("/sw/application/myapp/#0/current/myotherkey", ELEKTRA_KEY_META, "opt/arg", "required", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ksWithKeysFromTwoApps = elektraKeysetDup (ksOnlyWithKeysFromMyApp);
	elektraKeysetAppendKey (ksWithKeysFromTwoApps,
		     elektraKeyNew ("/sw/application/myotherapp/#0/current/somekey", ELEKTRA_KEY_META, "opt/arg", "required", ELEKTRA_KEY_END));
	elektraKeysetAppendKey (ksOnlyWithKeysFromMyApp,
		     elektraKeyNew ("/sw/application/myotherapp/#0/current/someotherkey", ELEKTRA_KEY_META, "opt/arg", "required", ELEKTRA_KEY_END));

	ElektraKey * parentKeyForMyApp = elektraKeyNew ("/sw/application/myapp/#0/current", ELEKTRA_KEY_END);

	char hash_ksWithKeysFroMyApp[65];
	char hash_ksWithKeysFromTwoApps[65];
	calculateSpecificationToken (hash_ksWithKeysFroMyApp, ksOnlyWithKeysFromMyApp, parentKeyForMyApp);
	calculateSpecificationToken (hash_ksWithKeysFromTwoApps, ksWithKeysFromTwoApps, parentKeyForMyApp);

	succeed_if (strcmp (hash_ksWithKeysFroMyApp, hash_ksWithKeysFromTwoApps) == 0,
		    "Token calculation did not properly cut out irrelevant keys! The hash for two different KeySets that have the same set "
		    "of Keys below the parentKey should be the same, but it was different!");

	elektraKeyDel (parentKeyForMyApp);
	elektraKeysetDel (ksOnlyWithKeysFromMyApp);
	elektraKeysetDel (ksWithKeysFromTwoApps);
}

int main (int argc, char ** argv)
{
	printf ("HASH    TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_keySet ();
	test_onlyKeysBelowParentKey ();

	printf ("\ntest_hash RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
