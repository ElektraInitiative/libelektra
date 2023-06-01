/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include <internal/config.h>
#endif

#include <stdio.h>

#include "./glob.h"
#include <fnmatch.h>
#include <tests.h>

#include <tests_plugin.h>

#define NR_KEYS 1

void test_match (void)
{
	succeed_if (fnmatch ("user:/*/to/key", "user:/path/to/key", FNM_PATHNAME) == 0, "could not do simple fnmatch");
}

void testKeys (KeySet * ks)
{
	Key * key = ksLookupByName (ks, "user:/tests/glob/test1", 0);
	exit_if_fail (key, "key user:/tests/glob/test1 not found");
	const Key * metaKey = keyGetMeta (key, "testmetakey1");
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

KeySet * createKeys (void)
{
	KeySet * ks = ksNew (30, keyNew ("user:/tests/glob/test1", KEY_END), keyNew ("user:/tests/glob/test2/subtest1", KEY_END),
			     keyNew ("user:/tests/glob/test3", KEY_END), KS_END);
	return ks;
}

void test_zeroMatchFlags (void)
{
	Key * parentKey = keyNew ("user:/tests/glob", KEY_END);
	KeySet * conf = ksNew (20, keyNew ("user:/glob/#1", KEY_VALUE, "*test1", KEY_META, "testmetakey1", "testvalue1", KEY_END),
			       /* disable default pathname globbing behaviour */
			       keyNew ("user:/glob/#1/flags", KEY_VALUE, "", KEY_END), KS_END);
	PLUGIN_OPEN ("glob");

	KeySet * ks = createKeys ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");


	Key * key = ksLookupByName (ks, "user:/tests/glob/test1", 0);
	exit_if_fail (key, "key user:/tests/glob/test1 not found");
	const Key * metaKey1 = keyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey1, "testmetakey1 not found");
	succeed_if (strcmp ("testvalue1", keyValue (metaKey1)) == 0, "value of metakey testmetakey1 not correct");

	key = ksLookupByName (ks, "user:/tests/glob/test3", 0);
	exit_if_fail (key, "user:/tests/glob/test3 not found");
	succeed_if (!keyGetMeta (key, "testmetakey1"), "testmetakey1 copied to wrong key");

	key = ksLookupByName (ks, "user:/tests/glob/test2/subtest1", 0);
	exit_if_fail (key, "user:/tests/glob/test2/subtest1 not found");
	const Key * metaKey2 = keyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey2, "testmetakey1 not found");
	succeed_if (strcmp ("testvalue1", keyValue (metaKey2)) == 0, "value of metakey testmetakey1 not correct");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void test_setGlobalMatch (void)
{
	Key * parentKey = keyNew ("user:/tests/glob", KEY_END);
	// clang-format off
	KeySet *conf = ksNew (20,
			keyNew ("user:/glob/#1", KEY_VALUE, "/*",
					KEY_META, "testmetakey1", "testvalue1",
					KEY_META, "testmetakey2", "testvalue2",
					KEY_END),
			KS_END);
	// clang-format on
	PLUGIN_OPEN ("glob");

	KeySet * ks = createKeys ();

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
	Key * parentKey = keyNew ("user:/tests/glob", KEY_END);
	// clang-format off
	KeySet *conf = ksNew (20,
			keyNew ("user:/glob/#1", KEY_VALUE, "/*",
					KEY_META, "testmetakey1", "testvalue1",
					KEY_META, "testmetakey2", "testvalue2",
					KEY_END),
			KS_END);
	// clang-format on
	PLUGIN_OPEN ("glob");

	KeySet * ks = createKeys ();

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
	Key * parentKey = keyNew ("user:/tests/glob", KEY_END);
	// clang-format off
	KeySet *conf = ksNew (20,
			keyNew ("user:/glob/get/#1", KEY_VALUE, "/*",
					KEY_META, "testmetakey1", "testvalue1",
					KEY_META, "testmetakey2", "testvalue2",
					KEY_END),
			keyNew ("user:/glob/set/#1", KEY_VALUE, "/*/*",
					KEY_META, "testmetakey1", "testvalue1",
					KEY_META, "testmetakey2", "testvalue2",
					KEY_END),
			KS_END);
	// clang-format on
	PLUGIN_OPEN ("glob");

	KeySet * ks = createKeys ();

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
	Key * parentKey = keyNew ("user:/tests/glob", KEY_END);
	// clang-format off
	KeySet *conf = ksNew (20,
			keyNew ("user:/glob/set/#1", KEY_VALUE, "/*",
					KEY_META, "testmetakey1", "testvalue1",
					KEY_META, "testmetakey2", "testvalue2",
					KEY_END),
			keyNew ("user:/glob/get/#1", KEY_VALUE, "/*/*",
					KEY_META, "testmetakey1", "testvalue1",
					KEY_META, "testmetakey2", "testvalue2",
					KEY_END),
			KS_END);
	// clang-format on
	PLUGIN_OPEN ("glob");

	KeySet * ks = createKeys ();

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
	Key * parentKey = keyNew ("user:/tests/glob", KEY_END);
	KeySet * conf =
		ksNew (20, keyNew ("user:/glob/#1", KEY_VALUE, "user:/tests/glob/*", KEY_META, "testmetakey1", "testvalue1", KEY_END),
		       /* explicitly request pathname matching */
		       keyNew ("user:/glob/#1/flags", KEY_VALUE, "pathname", KEY_END), KS_END);
	PLUGIN_OPEN ("glob");

	KeySet * ks = createKeys ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	Key * key = ksLookupByName (ks, "user:/tests/glob/test1", 0);
	exit_if_fail (key, "key user:/tests/glob/test1 not found");
	const Key * metaKey1 = keyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey1, "testmetakey1 not found");

	key = ksLookupByName (ks, "user:/tests/glob/test3", 0);
	exit_if_fail (key, "user:/tests/glob/test3 not found");
	const Key * metaKey2 = keyGetMeta (key, "testmetakey1");
	exit_if_fail (metaKey2, "testmetakey1 not found");

	key = ksLookupByName (ks, "user:/tests/glob/test2/subtest1", 0);
	exit_if_fail (key, "user:/tests/glob/test2/subtest1 not found");
	const Key * metaKey3 = keyGetMeta (key, "testmetakey1");
	exit_if_fail (!metaKey3, "testmetakey1 was copied to subtest1, but subtest1 should not be matched with pathname flag");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void test_onlyFirstMatchIsApplied (void)
{
	Key * parentKey = keyNew ("user:/tests/glob", KEY_END);
	// clang-format off
	KeySet * conf = ksNew (20,
				keyNew ("user:/glob/#1",
						KEY_VALUE, "user:/tests/glob/test1*",
						KEY_META, "testmetakey1", "testvalue1",
						KEY_END),
				keyNew ("user:/glob/#2",
						KEY_VALUE, "user:/tests/glob/*",
						KEY_META, "testmetakey2", "testvalue2",
						KEY_END),
			       /* disable all flags */
			    keyNew ("user:/glob/#1/flags",
			    		KEY_VALUE, "",
						KEY_END),
				keyNew ("user:/glob/#2/flags",
				   		KEY_VALUE, "",
						KEY_END),
				KS_END);
	// clang-format on
	PLUGIN_OPEN ("glob");

	KeySet * ks = createKeys ();

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	Key * key = ksLookupByName (ks, "user:/tests/glob/test1", 0);
	exit_if_fail (key, "key user:/tests/glob/test1 not found");
	const Key * firstMatchKey = keyGetMeta (key, "testmetakey1");
	exit_if_fail (firstMatchKey, "testmetakey1 not found");
	const Key * secondMatchKey = keyGetMeta (key, "testmetakey2");
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
