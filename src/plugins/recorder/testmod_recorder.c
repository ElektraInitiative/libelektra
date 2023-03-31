/**
 * @file
 *
 * @brief Tests for recorder plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew ("user:/tests/recorder", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("recorder");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

	succeed_if (plugin->kdbCommit (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbCommit was not successful");

	succeed_if (plugin->kdbError (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbError was not successful");

	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("RECORDER     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();

	print_result ("testmod_recorder");

	return nbError;
}