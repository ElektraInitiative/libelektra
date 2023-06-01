/**
 * @file
 *
 * @brief Tests for yamlcpp plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include "./yamlcpp.hpp"

#include <internal/kdbprivate.h>
#include <internal/plugin/load.h>
#include <internal/plugin/struct.h>
#include <internal/pluginload/module.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;
#include <tests.h>
#include <tests.hpp>

using ckdb::keyNew;

// -- Macros -------------------------------------------------------------------------------------------------------------------------------

#define OPEN_PLUGIN(parentName, filepath)                                                                                                  \
	kdb::KeySet modules{ 0, KS_END };                                                                                                  \
	kdb::KeySet config{ 0, KS_END };                                                                                                   \
	elektraModulesInit (modules.getKeySet (), 0);                                                                                      \
	kdb::Key parent{ parentName, KEY_VALUE, filepath, KEY_END };                                                                       \
	Plugin * plugin = elektraPluginOpen ("yamlcpp", modules.getKeySet (), config.getKeySet (), *parent);                               \
	exit_if_fail (plugin != NULL, "Could not open yamlcpp plugin")

#define CLOSE_PLUGIN()                                                                                                                     \
	elektraPluginClose (plugin, 0);                                                                                                    \
	elektraModulesClose (modules.getKeySet (), 0);                                                                                     \
	ksDel (modules.release ());                                                                                                        \
	config.release ()

/* We use this prefix in all header files that contain test data. */
#define PREFIX "user:/examples/yamlcpp/"

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

TEST (yamlcpp, contract) //! OCLint (avoid private static members)
#ifdef __llvm__
__attribute__ ((annotate ("oclint:suppress[too few branches in switch statement]"), annotate ("oclint:suppress[empty if statement]")))
#endif
{
	OPEN_PLUGIN ("system:/elektra/modules/yamlcpp", "file/path");

	kdb::KeySet keys;
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Unable to retrieve plugin contract");

	CLOSE_PLUGIN ();
}

static void test_read (string const & filename, kdb::KeySet expected, int const status = ELEKTRA_PLUGIN_STATUS_SUCCESS)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress")))
#endif
{
	string filepath = srcdir_file (filename.c_str ());

	OPEN_PLUGIN (PREFIX, filepath.c_str ());

	kdb::KeySet keys;
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), status, parent.getMeta<string> ("error/reason"));
	compare_keyset (keys, expected);

	CLOSE_PLUGIN ();
}

static void test_write_read (kdb::KeySet expected)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress")))
#endif
{
	string filepath = elektraFilename ();

	OPEN_PLUGIN (PREFIX, filepath.c_str ());

	// Write key set to file
	succeed_if_same (plugin->kdbSet (plugin, expected.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 parent.getMeta<string> ("error/reason"));

	// Read written data
	kdb::KeySet keySetRead;
	succeed_if_same (plugin->kdbGet (plugin, keySetRead.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 parent.getMeta<string> ("error/reason"));

	// Compare data
	compare_keyset (keySetRead, expected);

	// Clean up
	CLOSE_PLUGIN ();
}

TEST (yamlcpp, flat) //! OCLint (avoid private static members)
{
	test_read ("yamlcpp/flat_block_mapping.yaml",
#include "./yamlcpp/flat_block_mapping.hpp"
	);
	test_write_read (
#include "./yamlcpp/flat_block_mapping.hpp"
	);

	test_read ("yamlcpp/flat_flow_mapping.yaml",
#include "./yamlcpp/flat_flow_mapping.hpp"
	);
	test_write_read (
#include "./yamlcpp/flat_flow_mapping.hpp"
	);
}

TEST (yamlcpp, nested) //! OCLint (avoid private static members)
{
	test_read ("yamlcpp/nested_block_mapping.yaml",
#include "./yamlcpp/nested_block_mapping.hpp"
	);
	test_write_read (
#include "./yamlcpp/nested_block_mapping.hpp"
	);
	test_read ("yamlcpp/nested_mixed_mapping.yaml",
#include "./yamlcpp/nested_mixed_mapping.hpp"
	);
	test_write_read (
#include "./yamlcpp/nested_mixed_mapping.hpp"
	);
}

TEST (yamlcpp, array) //! OCLint (avoid private static members)
{
	test_read ("yamlcpp/simple_sequence.yaml",
#include "./yamlcpp/simple_sequence.hpp"
	);
	test_write_read (
#include "./yamlcpp/simple_sequence.hpp"
	);

	test_read ("yamlcpp/nested_sequences.yaml",
#include "./yamlcpp/nested_sequences.hpp"
	);
	test_write_read (
#include "./yamlcpp/nested_sequences.hpp"
	);
	test_write_read (
#include "./yamlcpp/mapping_with_array_key.hpp"
	);
	test_write_read (
#include "./yamlcpp/mixed.hpp"
	);
}

// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char * argv[])
{
	init (argc, argv); // Required for `srcdir_file` to work properly
	::testing::InitGoogleTest (&argc, argv);
	return RUN_ALL_TESTS ();
}
