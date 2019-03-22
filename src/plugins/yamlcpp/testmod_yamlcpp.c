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

/* We use this prefix in all header files that contain test data. */
#define PREFIX "user/examples/yamlcpp/"

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
	test_read ("yamlcpp/flat_block_mapping.yaml",
#include "yamlcpp/flat_block_mapping.h"
	);
	test_write_read (
#include "yamlcpp/flat_block_mapping.h"
	);

	test_read ("yamlcpp/flat_flow_mapping.yaml",
#include "yamlcpp/flat_flow_mapping.h"
	);
	test_write_read (
#include "yamlcpp/flat_flow_mapping.h"
	);
}

static void test_nested (void)
{
	test_read ("yamlcpp/nested_block_mapping.yaml",
#include "yamlcpp/nested_block_mapping.h"
	);
	test_write_read (
#include "yamlcpp/nested_block_mapping.h"
	);
	test_read ("yamlcpp/nested_mixed_mapping.yaml",
#include "yamlcpp/nested_mixed_mapping.h"
	);
	test_write_read (
#include "yamlcpp/nested_mixed_mapping.h"
	);
}

static void test_array (void)
{
	test_read ("yamlcpp/simple_sequence.yaml",
#include "yamlcpp/simple_sequence.h"
	);
	test_write_read (
#include "yamlcpp/simple_sequence.h"
	);

	test_read ("yamlcpp/nested_sequences.yaml",
#include "yamlcpp/nested_sequences.h"
	);
	test_write_read (
#include "yamlcpp/nested_sequences.h"
	);
}

static void test_map_array_key (void)
{
	test_write_read (
#include "yamlcpp/mapping_with_array_key.h"
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
	test_map_array_key ();

	print_result ("testmod_yamlcpp");

	return nbError;
}
