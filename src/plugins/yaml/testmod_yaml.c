/**
 * @file
 *
 * @brief Tests for yaml plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

static void test_basics ()
{
	printf ("• Test basic functionality of plugin\n");

	Key * parentKey = keyNew ("system/elektra/modules/yaml", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("yaml");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not retrieve plugin contract");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_get ()
{
	char const * const fileName = "yaml/simple.yaml";
	printf ("• Parse file “%s”\n", fileName);

	char const * const prefix = "user/yaml/tests/read";
	Key * parentKey = keyNew (prefix, KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("yaml");

	KeySet * keySet = ksNew (0, KS_END);

	int status = plugin->kdbGet (plugin, keySet, parentKey);

	succeed_if (status == ELEKTRA_PLUGIN_STATUS_SUCCESS || status == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "Unable to open or parse file");
	succeed_if (output_error (parentKey), "Received unexpected error while reading the configuration");

	keyDel (parentKey);
	ksDel (keySet);
	PLUGIN_CLOSE ();
}

// ========
// = Main =
// ========

int main (int argc, char ** argv)
{
	printf ("🐪 YAML Tests\n");
	printf ("==============\n\n");

	init (argc, argv);

	test_basics ();
	test_get ();

	printf ("\nResults: %d Test%s done — %d error%s.\n", nbTest, nbTest != 1 ? "s" : "", nbError, nbError != 1 ? "s" : "");

	return nbError;
}
