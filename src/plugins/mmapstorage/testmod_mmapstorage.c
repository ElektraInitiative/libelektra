/**
 * @file
 *
 * @brief Tests for mmapstorage plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>


//static void test_basics ()
//{
//	printf ("test basics\n");
//
//	Key * parentKey = keyNew ("user/tests/mmapstorage", KEY_END);
//	KeySet * conf = ksNew (0, KS_END);
//	PLUGIN_OPEN ("mmapstorage");
//
//	KeySet * ks = ksNew (0, KS_END);
//
//	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
//
//	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");
//
//	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");
//
//	succeed_if (plugin->kdbError (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbError was not successful");
//
//	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");
//
//	keyDel (parentKey);
//	ksDel (ks);
//	PLUGIN_CLOSE ();
//}

static KeySet *simpleTestKeySet () {
	return ksNew (10,
		      keyNew ("user/tests/mmapstorage/simpleKey", KEY_VALUE, "root key", KEY_END),
		      keyNew ("user/tests/mmapstorage/simpleKey/a", KEY_VALUE, "a value", KEY_END),
		      keyNew ("user/tests/mmapstorage/simpleKey/b", KEY_VALUE, "b value", KEY_END),
		      KS_END
	);
}

static KeySet *metaTestKeySet () {
	return ksNew (10,
		      keyNew ("user/tests/mmapstorage",
			      KEY_VALUE, "root key",
			      KEY_META, "a", "b",
			      KEY_END),
		      keyNew ("user/tests/mmapstorage/a",
			      KEY_VALUE, "a value",
			      KEY_META, "ab", "cd",
			      KEY_END),
		      keyNew ("user/tests/mmapstorage/b",
			      KEY_VALUE, "b value",
			      KEY_META, "longer val", "here some even more with ugly €@\\1¹²³¼ chars",
			      KEY_END),
		      KS_END
	);
}

static void test_mmap_set_get (const char * tmpFile)
{
	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = simpleTestKeySet ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	KeySet * returned = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");
	KeySet * expected = simpleTestKeySet ();
	compare_keyset(returned, expected);

	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_get_after_reopen (const char * tmpFile)
{
	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * returned = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = simpleTestKeySet ();
	compare_keyset(returned, expected);
	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_mmap_empty_after_clear (const char * tmpFile)
{
	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * returned = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	succeed_if (ksGetSize(returned) == 0, "KeySet not empty after clear (or nullptr)");

	ksDel (returned);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_mmapMeta (const char * tmpFile)
{
	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = metaTestKeySet();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	KeySet * returned = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = metaTestKeySet ();
	compare_keyset(expected, returned);
//	printf ("ks:\n");
//	output_keyset (ks);
//	printf ("expected:\n");
//	output_keyset (expected);
	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}
//
static void test_mmapMeta_reRead (const char * tmpFile)
{
	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = metaTestKeySet ();
	compare_keyset(expected, ks);
//	printf ("ks:\n");
//	output_keyset (ks);
//	printf ("expected:\n");
//	output_keyset (expected);
	ksDel (expected);
	
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void clearStorage (const char * tmpFile)
{
	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}



int main (int argc, char ** argv)
{
	printf ("MMAPSTORAGE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	const char * tmpFile = elektraFilename();

	test_mmap_set_get (tmpFile);
	test_mmap_get_after_reopen (tmpFile);

	clearStorage (tmpFile);

	test_mmap_empty_after_clear (tmpFile);
	
	test_mmapMeta (tmpFile);
	test_mmapMeta_reRead (tmpFile);

	// TODO: test keyCopyMeta


	printf ("\ntestmod_mmapstorage RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
