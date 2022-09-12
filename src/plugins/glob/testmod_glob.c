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
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/glob/test1", 0);
	exit_if_fail (key, "key user:/tests/glob/test1 not found");
	const ElektraKey * metaKey = elektraKeyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey, "testmetakey1 not found");
	succeed_if (strcmp ("testvalue1", elektraKeyValue (metaKey)) == 0, "value of metakey testmetakey1 not correct");
	metaKey = elektraKeyGetMeta (key, "testmetakey2");
	exit_if_fail (metaKey, "testmetakey2 not found");
	succeed_if (strcmp ("testvalue2", elektraKeyValue (metaKey)) == 0, "value of metakey testmetakey2 not correct");

	key = elektraKeysetLookupByName (ks, "user:/tests/glob/test2/subtest1", 0);
	exit_if_fail (key, "key user:/test1/subtest1 not found");
	succeed_if (!elektraKeyGetMeta (key, "testmetakey1"), "testmetakey1 copied to wrong key");
	succeed_if (!elektraKeyGetMeta (key, "testmetakey2"), "testmetakey2 copied to wrong key");

	key = elektraKeysetLookupByName (ks, "user:/tests/glob/test3", 0);
	exit_if_fail (key, "key user:/tests/glob/test3 not found");
	metaKey = elektraKeyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey, "testmetakey1 not found");
	succeed_if (strcmp ("testvalue1", elektraKeyValue (metaKey)) == 0, "value of metakey testmetakey1 not correct");
	metaKey = elektraKeyGetMeta (key, "testmetakey2");
	exit_if_fail (metaKey, "testmetakey2 not found");
	succeed_if (strcmp ("testvalue2", elektraKeyValue (metaKey)) == 0, "value of metakey testmetakey2 not correct");
}

ElektraKeyset * createKeys (void)
{
	ElektraKeyset * ks = elektraKeysetNew (30, elektraKeyNew ("user:/tests/glob/test1", ELEKTRA_KEY_END), elektraKeyNew ("user:/tests/glob/test2/subtest1", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/glob/test3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	return ks;
}

void test_zeroMatchFlags (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("user:/glob/#1", ELEKTRA_KEY_VALUE, "*test1", ELEKTRA_KEY_META, "testmetakey1", "testvalue1", ELEKTRA_KEY_END),
			       /* disable default pathname globbing behaviour */
			       elektraKeyNew ("user:/glob/#1/flags", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("glob");

	ElektraKeyset * ks = createKeys ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");


	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/glob/test1", 0);
	exit_if_fail (key, "key user:/tests/glob/test1 not found");
	const ElektraKey * metaKey1 = elektraKeyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey1, "testmetakey1 not found");
	succeed_if (strcmp ("testvalue1", elektraKeyValue (metaKey1)) == 0, "value of metakey testmetakey1 not correct");

	key = elektraKeysetLookupByName (ks, "user:/tests/glob/test3", 0);
	exit_if_fail (key, "user:/tests/glob/test3 not found");
	succeed_if (!elektraKeyGetMeta (key, "testmetakey1"), "testmetakey1 copied to wrong key");

	key = elektraKeysetLookupByName (ks, "user:/tests/glob/test2/subtest1", 0);
	exit_if_fail (key, "user:/tests/glob/test2/subtest1 not found");
	const ElektraKey * metaKey2 = elektraKeyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey2, "testmetakey1 not found");
	succeed_if (strcmp ("testvalue1", elektraKeyValue (metaKey2)) == 0, "value of metakey testmetakey1 not correct");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

void test_setGlobalMatch (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	// clang-format off
	ElektraKeyset *conf = elektraKeysetNew (20,
			elektraKeyNew ("user:/glob/#1", ELEKTRA_KEY_VALUE, "/*",
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
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_getGlobalMatch (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	// clang-format off
	ElektraKeyset *conf = elektraKeysetNew (20,
			elektraKeyNew ("user:/glob/#1", ELEKTRA_KEY_VALUE, "/*",
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
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_getDirectionMatch (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	// clang-format off
	ElektraKeyset *conf = elektraKeysetNew (20,
			elektraKeyNew ("user:/glob/get/#1", ELEKTRA_KEY_VALUE, "/*",
					ELEKTRA_KEY_META, "testmetakey1", "testvalue1",
					ELEKTRA_KEY_META, "testmetakey2", "testvalue2",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/glob/set/#1", ELEKTRA_KEY_VALUE, "/*/*",
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
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_setDirectionMatch (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	// clang-format off
	ElektraKeyset *conf = elektraKeysetNew (20,
			elektraKeyNew ("user:/glob/set/#1", ELEKTRA_KEY_VALUE, "/*",
					ELEKTRA_KEY_META, "testmetakey1", "testvalue1",
					ELEKTRA_KEY_META, "testmetakey2", "testvalue2",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/glob/get/#1", ELEKTRA_KEY_VALUE, "/*/*",
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
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_namedMatchFlags (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	ElektraKeyset * conf =
		elektraKeysetNew (20, elektraKeyNew ("user:/glob/#1", ELEKTRA_KEY_VALUE, "user:/tests/glob/*", ELEKTRA_KEY_META, "testmetakey1", "testvalue1", ELEKTRA_KEY_END),
		       /* explicitly request pathname matching */
		       elektraKeyNew ("user:/glob/#1/flags", ELEKTRA_KEY_VALUE, "pathname", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("glob");

	ElektraKeyset * ks = createKeys ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/glob/test1", 0);
	exit_if_fail (key, "key user:/tests/glob/test1 not found");
	const ElektraKey * metaKey1 = elektraKeyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey1, "testmetakey1 not found");

	key = elektraKeysetLookupByName (ks, "user:/tests/glob/test3", 0);
	exit_if_fail (key, "user:/tests/glob/test3 not found");
	const ElektraKey * metaKey2 = elektraKeyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey2, "testmetakey1 not found");

	key = elektraKeysetLookupByName (ks, "user:/tests/glob/test2/subtest1", 0);
	exit_if_fail (key, "user:/tests/glob/test2/subtest1 not found");
	const ElektraKey * metaKey3 = elektraKeyGetMeta (key, "testmetakey1");
	exit_if_fail (!metaKey3, "testmetakey1 was copied to subtest1, but subtest1 should not be matched with pathname flag");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

void test_onlyFirstMatchIsApplied (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/glob", ELEKTRA_KEY_END);
	// clang-format off
	ElektraKeyset * conf = elektraKeysetNew (20,
				elektraKeyNew ("user:/glob/#1",
						ELEKTRA_KEY_VALUE, "user:/tests/glob/test1*",
						ELEKTRA_KEY_META, "testmetakey1", "testvalue1",
						ELEKTRA_KEY_END),
				elektraKeyNew ("user:/glob/#2",
						ELEKTRA_KEY_VALUE, "user:/tests/glob/*",
						ELEKTRA_KEY_META, "testmetakey2", "testvalue2",
						ELEKTRA_KEY_END),
			       /* disable all flags */
			    elektraKeyNew ("user:/glob/#1/flags",
			    		ELEKTRA_KEY_VALUE, "",
						ELEKTRA_KEY_END),
				elektraKeyNew ("user:/glob/#2/flags",
				   		ELEKTRA_KEY_VALUE, "",
						ELEKTRA_KEY_END),
				ELEKTRA_KS_END);
	// clang-format on
	PLUGIN_OPEN ("glob");

	ElektraKeyset * ks = createKeys ();

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/glob/test1", 0);
	exit_if_fail (key, "key user:/tests/glob/test1 not found");
	const ElektraKey * firstMatchKey = elektraKeyGetMeta (key, "testmetakey1");
	exit_if_fail (firstMatchKey, "testmetakey1 not found");
	const ElektraKey * secondMatchKey = elektraKeyGetMeta (key, "testmetakey2");
	exit_if_fail (!secondMatchKey, "testmetakey2 was applied to testmetakey1 although another match was already applied")

		key = elektraKeysetLookupByName (ks, "user:/tests/glob/test2/subtest1", 0);
	exit_if_fail (key, "user:/tests/glob/test2/subtest1 not found");
	exit_if_fail (elektraKeyGetMeta (key, "testmetakey2"), "testmetakey2 not found");

	key = elektraKeysetLookupByName (ks, "user:/tests/glob/test3", 0);
	exit_if_fail (key, "user:/tests/glob/test3 not found");
	exit_if_fail (elektraKeyGetMeta (key, "testmetakey2"), "testmetakey2 not found");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
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
