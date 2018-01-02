/**
 * @file
 *
 * @brief Tests for semlock plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_OpenClose (void)
{
	printf ("test Open & Close\n");

	Key * parentKey = keyNew ("user/tests/semlock", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("semlock");

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_GetSet (void)
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

	// check if /dev/shm is a mounted tempfs
	if (system ("mount | grep /dev/shm | grep -q tmpfs"))
	{
		fprintf (stderr, "/dev/shm not mounted as tempfs, look in the README of semlock for more info!\n");
		return 1;
	}

	test_OpenClose ();
	test_GetSet ();

	print_result ("testmod_semlock");

	return nbError;
}
