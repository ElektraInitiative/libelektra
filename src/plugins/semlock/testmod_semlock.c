/**
 * @file
 *
 * @brief Tests for semlock plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_OpenClose ()
{
	printf ("test Open & Close\n");

	Key * parentKey = keyNew ("user/tests/semlock", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("semlock");

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_GetSet ()
{
	printf ("test Get & Set\n");

	Key * parentKey = keyNew ("user/tests/semlock", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("semlock");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");


	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("SEMLOCK     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_OpenClose ();
	test_GetSet ();

	printf ("\ntestmod_semlock RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
