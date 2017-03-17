/**
 * @file
 *
 * @brief Tests for mini plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_basics ()
{
	printf ("Test basic functionality of plugin\n");

	Key * parentKey = keyNew ("system/elektra/modules/mini", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mini");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "Could not retrieve plugin contract");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("mINI Tests 🚙\n");
	printf ("==============\n\n");

	init (argc, argv);

	test_basics ();

	printf ("\ntestmod_mini RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
