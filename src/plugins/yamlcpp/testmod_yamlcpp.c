/**
 * @file
 *
 * @brief Tests for yamlcpp plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

// -- Macros -------------------------------------------------------------------------------------------------------------------------------

#define INIT_PLUGIN(parent, filepath)                                                                                                      \
	Key * parentKey = keyNew (parent, KEY_VALUE, filepath, KEY_END);                                                                   \
	KeySet * conf = ksNew (0, KS_END);                                                                                                 \
	PLUGIN_OPEN ("yamlcpp");                                                                                                           \
	KeySet * keySet = ksNew (0, KS_END)

#define CLOSE_PLUGIN()                                                                                                                     \
	keyDel (parentKey);                                                                                                                \
	ksDel (keySet);                                                                                                                    \
	PLUGIN_CLOSE ()

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

static void test_contract (void) __attribute__ ((annotate ("oclint:suppress[high ncss method]")))
{
	printf ("• Retrieve plugin contract\n");

	INIT_PLUGIN ("system/elektra/modules/yamlcpp", "");

	succeed_if (plugin->kdbGet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not retrieve plugin contract");

	CLOSE_PLUGIN ();
}

static void test_read (void) __attribute__ ((annotate ("oclint:suppress[high ncss method]")))
{
	printf ("• Retrieve data\n");

	INIT_PLUGIN ("user/examples/yamlcpp", srcdir_file ("yamlcpp/test.yaml"));

	plugin->kdbGet (plugin, keySet, parentKey);

	CLOSE_PLUGIN ();
}

// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char ** argv)
{
	printf ("🐪 YAML CPP   TESTS\n");
	printf ("====================\n\n");

	init (argc, argv);

	test_contract ();
	test_read ();

	printf ("\nResults: %d Test%s done — %d error%s.\n", nbTest, nbTest == 1 ? "" : "s", nbError, nbError == 1 ? "" : "s");

	return nbError;
}
