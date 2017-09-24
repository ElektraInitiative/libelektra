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
	PLUGIN_OPEN ("yamlcpp")

#define INIT_PLUGIN_GET(parent, filepath, errorMessage)                                                                                    \
	INIT_PLUGIN (parent, filepath);                                                                                                    \
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

	INIT_PLUGIN_GET ("system/elektra/modules/yamlcpp", "", "Could not retrieve plugin contract");
	CLOSE_PLUGIN ();
}

static void test_read (char const * const filepath, KeySet * const expected)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress")))
#endif
{
	printf ("• Retrieve data from file “%s”\n", filepath);

	INIT_PLUGIN_GET ("user/examples/yamlcpp", srcdir_file (filepath), "Unable to open or parse file");

	compare_keyset (keySet, expected);

	ksDel (expected);
	CLOSE_PLUGIN ();
}

static void test_write_read (KeySet * const keySet)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress")))
#endif
{
	printf ("• Write data, read data, and compare results\n");

	INIT_PLUGIN ("user/examples/yamlcpp", elektraFilename ());

	// Write key set to file
	succeed_if (plugin->kdbSet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Unable to write to file");

	// Read written data
	KeySet * keySetRead = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, keySetRead, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Unable to open or parse file");

	// Compare data
	compare_keyset (keySet, keySetRead);

	// Clean up
	ksDel (keySetRead);
	CLOSE_PLUGIN ();
}

static void test_flat (void)
{
	test_read ("yamlcpp/Flat Block Mapping.yaml",
#include "yamlcpp/Flat Block Mapping.h"
		   );
	test_write_read (
#include "yamlcpp/Flat Block Mapping.h"
		);

	test_read ("yamlcpp/Flat Flow Mapping.yaml",
#include "yamlcpp/Flat Flow Mapping.h"
		   );
	test_write_read (
#include "yamlcpp/Flat Flow Mapping.h"
		);
}

static void test_nested (void)
{
	test_read ("yamlcpp/Nested Block Mapping.yaml",
#include "yamlcpp/Nested Block Mapping.h"
		   );
	test_write_read (
#include "yamlcpp/Nested Block Mapping.h"
		);
	test_read ("yamlcpp/Nested Mixed Mapping.yaml",
#include "yamlcpp/Nested Mixed Mapping.h"
		   );
	test_write_read (
#include "yamlcpp/Nested Mixed Mapping.h"
		);
}

static void test_array (void)
{
	test_read ("yamlcpp/Simple Sequence.yaml",
#include "yamlcpp/Simple Sequence.h"
		   );
	test_write_read (
#include "yamlcpp/Simple Sequence.h"
		);

	test_read ("yamlcpp/Nested Sequences.yaml",
#include "yamlcpp/Nested Sequences.h"
		   );
	test_write_read (
#include "yamlcpp/Nested Sequences.h"
		);
}

// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char ** argv)
{
	printf ("🐪 YAML CPP   TESTS\n");
	printf ("====================\n\n");

	init (argc, argv);

	test_contract ();
	test_flat ();
	test_nested ();
	test_array ();

	printf ("\nResults: %d Test%s done — %d error%s.\n", nbTest, nbTest == 1 ? "" : "s", nbError, nbError == 1 ? "" : "s");

	return nbError;
}
