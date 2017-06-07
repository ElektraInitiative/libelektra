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

static KeySet * simpleTestKeySet ()
{
	return ksNew(10,
		  keyNew("user/tests/mmapstorage/simpleKey", KEY_VALUE, "root key", KEY_END),
		  keyNew("user/tests/mmapstorage/simpleKey/a", KEY_VALUE, "a value", KEY_END),
		  keyNew("user/tests/mmapstorage/simpleKey/b", KEY_VALUE, "b value", KEY_END),
		  KS_END
	);
}

static void test_mmapWrite ()
{
	const char * tmpFile = elektraFilename();

	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = simpleTestKeySet ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	keyDel (parentKey);
	//ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmapRead ()
{
	const char * tmpFile = elektraFilename();

	Key * parentKey = keyNew("user/tests/mmapstorage", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = ksNew (0, KS_END);

	//succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ksAppend(ks, simpleTestKeySet ());

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = simpleTestKeySet ();
	compare_keyset(ks, expected);
	printf ("ks:\n");
	output_keyset (ks);
	printf ("expected:\n");
	output_keyset (expected);
	ksDel (expected);

	keyDel (parentKey);
	//ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("MMAPSTORAGE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	//test_basics ();
	test_mmapWrite ();
	test_mmapRead ();

	printf ("\ntestmod_mmapstorage RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
