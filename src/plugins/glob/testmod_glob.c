/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>

#include "glob.h"
#include <fnmatch.h>
#include <tests.h>

#include <tests_plugin.h>

#define NR_KEYS 1

void test_match (void)
{
	succeed_if (fnmatch ("user:/*/to/key", "user:/path/to/key", FNM_PATHNAME) == 0, "could not do simple fnmatch");
}

void testKeys (ElektraKeyset * ks)
{
	ElektraKey * key = ksLookupByName (ks, "user:/tests/glob/test1", 0);
	exit_if_fail (key, "key user:/tests/glob/test1 not found");
	const ElektraKey * metaKey = keyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey, "testmetakey1 not found");
	succeed_if (strcmp ("testvalue1", keyValue (metaKey)) == 0, "value of metakey testmetakey1 not correct");
	metaKey = keyGetMeta (key, "testmetakey2");
	exit_if_fail (metaKey, "testmetakey2 not found");
	succeed_if (strcmp ("testvalue2", keyValue (metaKey)) == 0, "value of metakey testmetakey2 not correct");

	key = ksLookupByName (ks, "user:/tests/glob/test2/subtest1", 0);
	exit_if_fail (key, "key user:/test1/subtest1 not found");
	succeed_if (!keyGetMeta (key, "testmetakey1"), "testmetakey1 copied to wrong key");
	succeed_if (!keyGetMeta (key, "testmetakey2"), "testmetakey2 copied to wrong key");

	key = ksLookupByName (ks, "user:/tests/glob/test3", 0);
	exit_if_fail (key, "key user:/tests/glob/test3 not found");
	metaKey = keyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey, "testmetakey1 not found");
	succeed_if (strcmp ("testvalue1", keyValue (metaKey)) == 0, "value of metakey testmetakey1 not correct");
	metaKey = keyGetMeta (key, "testmetakey2");
	exit_if_fail (metaKey, "testmetakey2 not found");
	succeed_if (strcmp ("testvalue2", keyValue (metaKey)) == 0, "value of metakey testmetakey2 not correct");
}

ElektraKeyset * createKeys (void)
{
	ElektraKeyset * ks = ksNew (30, keyNew ("user:/tests/glob/test1", ELEKTRA_KEY_END), keyNew ("user:/tests/glob/test2/subtest1", ELEKTRA_KEY_END),
			     keyNew ("user:/tests/glob/test3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	return ks;
}

void test_zeroMatchFlags (void)
{
	ElektraKey * parentKey = keyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (20, keyNew ("user:/glob/#1", ELEKTRA_KEY_VALUE, "*test1", ELEKTRA_KEY_META, "testmetakey1", "testvalue1", ELEKTRA_KEY_END),
			       /* disable default pathname globbing behaviour */
			       keyNew ("user:/glob/#1/flags", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("glob");

	ElektraKeyset * ks = createKeys ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");


	ElektraKey * key = ksLookupByName (ks, "user:/tests/glob/test1", 0);
	exit_if_fail (key, "key user:/tests/glob/test1 not found");
	const ElektraKey * metaKey1 = keyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey1, "testmetakey1 not found");
	succeed_if (strcmp ("testvalue1", keyValue (metaKey1)) == 0, "value of metakey testmetakey1 not correct");

	key = ksLookupByName (ks, "user:/tests/glob/test3", 0);
	exit_if_fail (key, "user:/tests/glob/test3 not found");
	succeed_if (!keyGetMeta (key, "testmetakey1"), "testmetakey1 copied to wrong key");

	key = ksLookupByName (ks, "user:/tests/glob/test2/subtest1", 0);
	exit_if_fail (key, "user:/tests/glob/test2/subtest1 not found");
	const ElektraKey * metaKey2 = keyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey2, "testmetakey1 not found");
	succeed_if (strcmp ("testvalue1", keyValue (metaKey2)) == 0, "value of metakey testmetakey1 not correct");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void test_setGlobalMatch (void)
{
	ElektraKey * parentKey = keyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	// clang-format off
	ElektraKeyset *conf = ksNew (20,
			keyNew ("user:/glob/#1", ELEKTRA_KEY_VALUE, "/*",
					ELEKTRA_KEY_META, "testmetakey1", "testvalue1",
					ELEKTRA_KEY_META, "testmetakey2", "testvalue2",
					ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
	// clang-format on
	PLUGIN_OPEN ("glob");

	ElektraKeyset * ks = createKeys ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	testKeys (ks);
	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_getGlobalMatch (void)
{
	ElektraKey * parentKey = keyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	// clang-format off
	ElektraKeyset *conf = ksNew (20,
			keyNew ("user:/glob/#1", ELEKTRA_KEY_VALUE, "/*",
					ELEKTRA_KEY_META, "testmetakey1", "testvalue1",
					ELEKTRA_KEY_META, "testmetakey2", "testvalue2",
					ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
	// clang-format on
	PLUGIN_OPEN ("glob");

	ElektraKeyset * ks = createKeys ();

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	testKeys (ks);
	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_getDirectionMatch (void)
{
	ElektraKey * parentKey = keyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	// clang-format off
	ElektraKeyset *conf = ksNew (20,
			keyNew ("user:/glob/get/#1", ELEKTRA_KEY_VALUE, "/*",
					ELEKTRA_KEY_META, "testmetakey1", "testvalue1",
					ELEKTRA_KEY_META, "testmetakey2", "testvalue2",
					ELEKTRA_KEY_END),
			keyNew ("user:/glob/set/#1", ELEKTRA_KEY_VALUE, "/*/*",
					ELEKTRA_KEY_META, "testmetakey1", "testvalue1",
					ELEKTRA_KEY_META, "testmetakey2", "testvalue2",
					ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
	// clang-format on
	PLUGIN_OPEN ("glob");

	ElektraKeyset * ks = createKeys ();

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	testKeys (ks);
	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_setDirectionMatch (void)
{
	ElektraKey * parentKey = keyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	// clang-format off
	ElektraKeyset *conf = ksNew (20,
			keyNew ("user:/glob/set/#1", ELEKTRA_KEY_VALUE, "/*",
					ELEKTRA_KEY_META, "testmetakey1", "testvalue1",
					ELEKTRA_KEY_META, "testmetakey2", "testvalue2",
					ELEKTRA_KEY_END),
			keyNew ("user:/glob/get/#1", ELEKTRA_KEY_VALUE, "/*/*",
					ELEKTRA_KEY_META, "testmetakey1", "testvalue1",
					ELEKTRA_KEY_META, "testmetakey2", "testvalue2",
					ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
	// clang-format on
	PLUGIN_OPEN ("glob");

	ElektraKeyset * ks = createKeys ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	testKeys (ks);
	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_namedMatchFlags (void)
{
	ElektraKey * parentKey = keyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	ElektraKeyset * conf =
		ksNew (20, keyNew ("user:/glob/#1", ELEKTRA_KEY_VALUE, "user:/tests/glob/*", ELEKTRA_KEY_META, "testmetakey1", "testvalue1", ELEKTRA_KEY_END),
		       /* explicitly request pathname matching */
		       keyNew ("user:/glob/#1/flags", ELEKTRA_KEY_VALUE, "pathname", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("glob");

	ElektraKeyset * ks = createKeys ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	ElektraKey * key = ksLookupByName (ks, "user:/tests/glob/test1", 0);
	exit_if_fail (key, "key user:/tests/glob/test1 not found");
	const ElektraKey * metaKey1 = keyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey1, "testmetakey1 not found");

	key = ksLookupByName (ks, "user:/tests/glob/test3", 0);
	exit_if_fail (key, "user:/tests/glob/test3 not found");
	const ElektraKey * metaKey2 = keyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey2, "testmetakey1 not found");

	key = ksLookupByName (ks, "user:/tests/glob/test2/subtest1", 0);
	exit_if_fail (key, "user:/tests/glob/test2/subtest1 not found");
	const ElektraKey * metaKey3 = keyGetMeta (key, "testmetakey1");
	exit_if_fail (!metaKey3, "testmetakey1 was copied to subtest1, but subtest1 should not be matched with pathname flag");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void test_onlyFirstMatchIsApplied (void)
{
	ElektraKey * parentKey = keyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	// clang-format off
	ElektraKeyset * conf = ksNew (20,
				keyNew ("user:/glob/#1",
						ELEKTRA_KEY_VALUE, "user:/tests/glob/test1*",
						ELEKTRA_KEY_META, "testmetakey1", "testvalue1",
						ELEKTRA_KEY_END),
				keyNew ("user:/glob/#2",
						ELEKTRA_KEY_VALUE, "user:/tests/glob/*",
						ELEKTRA_KEY_META, "testmetakey2", "testvalue2",
						ELEKTRA_KEY_END),
			       /* disable all flags */
			    keyNew ("user:/glob/#1/flags",
			    		ELEKTRA_KEY_VALUE, "",
						ELEKTRA_KEY_END),
				keyNew ("user:/glob/#2/flags",
				   		ELEKTRA_KEY_VALUE, "",
						ELEKTRA_KEY_END),
				ELEKTRA_KS_END);
	// clang-format on
	PLUGIN_OPEN ("glob");

	ElektraKeyset * ks = createKeys ();

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	ElektraKey * key = ksLookupByName (ks, "user:/tests/glob/test1", 0);
	exit_if_fail (key, "key user:/tests/glob/test1 not found");
	const ElektraKey * firstMatchKey = keyGetMeta (key, "testmetakey1");
	exit_if_fail (firstMatchKey, "testmetakey1 not found");
	const ElektraKey * secondMatchKey = keyGetMeta (key, "testmetakey2");
	exit_if_fail (!secondMatchKey, "testmetakey2 was applied to testmetakey1 although another match was already applied")

		key = ksLookupByName (ks, "user:/tests/glob/test2/subtest1", 0);
	exit_if_fail (key, "user:/tests/glob/test2/subtest1 not found");
	exit_if_fail (keyGetMeta (key, "testmetakey2"), "testmetakey2 not found");

	key = ksLookupByName (ks, "user:/tests/glob/test3", 0);
	exit_if_fail (key, "user:/tests/glob/test3 not found");
	exit_if_fail (keyGetMeta (key, "testmetakey2"), "testmetakey2 not found");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("GLOB      TESTS\n");
	printf ("====================\n\n");

	init (argc, argv);

	test_match ();
	test_zeroMatchFlags ();
	test_setGlobalMatch ();
	test_setDirectionMatch ();
	test_getGlobalMatch ();
	test_getDirectionMatch ();
	test_namedMatchFlags ();
	test_onlyFirstMatchIsApplied ();

	print_result ("testmod_glob");

	return nbError;
}
