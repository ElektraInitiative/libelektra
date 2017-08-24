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

#define INIT_PLUGIN(parent, filepath, errorMessage)                                                                                        \
	Key * parentKey = keyNew (parent, KEY_VALUE, filepath, KEY_END);                                                                   \
	KeySet * conf = ksNew (0, KS_END);                                                                                                 \
	PLUGIN_OPEN ("yamlcpp");                                                                                                           \
	KeySet * keySet = ksNew (0, KS_END);                                                                                               \
	succeed_if (plugin->kdbGet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, errorMessage)

#define CLOSE_PLUGIN()                                                                                                                     \
	keyDel (parentKey);                                                                                                                \
	ksDel (keySet);                                                                                                                    \
	PLUGIN_CLOSE ()

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

static void test_contract (void)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]")))
#endif
{
	printf ("• Retrieve plugin contract\n");

	INIT_PLUGIN ("system/elektra/modules/yamlcpp", "", "Could not retrieve plugin contract");
	CLOSE_PLUGIN ();
}

static void test_read (char const * const filepath, KeySet const * const expected)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress")))
#endif
{
	printf ("• Retrieve data from file “%s”\n", filepath);

	INIT_PLUGIN ("user/examples/yamlcpp", srcdir_file (filepath), "Unable to open or parse file");

	compare_keyset (keySet, expected);

	CLOSE_PLUGIN ();
}

static void test_write (char const * const filepath, KeySet * const keySet)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]")))
#endif
{
	printf ("• Write data and compare result with “%s”\n", filepath);

	Key * parentKey = keyNew ("user/examples/yamlcpp", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("yamlcpp");
	succeed_if (plugin->kdbSet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Unable to write to file");

	succeed_if (compare_line_files (srcdir_file (filepath), keyString (parentKey)),
		    "Output of plugin does not match the expected output");

	CLOSE_PLUGIN ();
}

// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char ** argv)
{
	printf ("🐪 YAML CPP   TESTS\n");
	printf ("====================\n\n");

	init (argc, argv);

	test_contract ();
	test_read ("yamlcpp/Flat Block Mapping.yaml",
#include "yamlcpp/Flat Block Mapping.h"
		   );
	test_read ("yamlcpp/Flat Flow Mapping.yaml",
#include "yamlcpp/Flat Flow Mapping.h"
		   );
	test_write ("yamlcpp/Output.yaml",
#include "yamlcpp/Flat Flow Mapping.h"
		    );

	printf ("\nResults: %d Test%s done — %d error%s.\n", nbTest, nbTest == 1 ? "" : "s", nbError, nbError == 1 ? "" : "s");

	return nbError;
}
