/**
 * @file
 *
 * @brief Tests for yaml plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_basics ()
{
	printf ("• Test basic functionality of plugin\n");

	Key * parentKey = keyNew ("user/tests/yaml", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("yaml");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "Call of kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "Call of kdbSet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("🐪 YAML Tests\n");
	printf ("==============\n\n");

	init (argc, argv);

	test_basics ();

	printf ("\nResults: %d Test%s done — %d error%s.\n", nbTest, nbTest != 1 ? "s" : "", nbError, nbError != 1 ? "s" : "");

	return nbError;
}
