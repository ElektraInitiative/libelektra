/**
 * @file
 *
 * @brief Tests for yamlcpp plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include "yamlcpp.hpp"

#include <kdbmodule.h>
#include <kdbprivate.h>

#include <tests.h>
#include <tests.hpp>

using ckdb::keyNew;

using CppKeySet = kdb::KeySet;
using CppKey = kdb::Key;

// -- Macros -------------------------------------------------------------------------------------------------------------------------------

#define OPEN_PLUGIN(parentName, filepath)                                                                                                  \
	CppKeySet modules{ 0, KS_END };                                                                                                    \
	CppKeySet config{ 0, KS_END };                                                                                                     \
	elektraModulesInit (modules.getKeySet (), 0);                                                                                      \
	CppKey parent{ parentName, KEY_VALUE, filepath, KEY_END };                                                                         \
	Plugin * plugin = elektraPluginOpen ("yamlcpp", modules.getKeySet (), config.getKeySet (), *parent);                               \
	exit_if_fail (plugin != NULL, "Could not open yamlcpp plugin")

#define CLOSE_PLUGIN()                                                                                                                     \
	elektraPluginClose (plugin, 0);                                                                                                    \
	elektraModulesClose (modules.getKeySet (), 0);                                                                                     \
	ksDel (modules.release ());                                                                                                        \
	config.release ()

/* We use this prefix in all header files that contain test data. */
#define PREFIX "user/examples/yamlcpp/"

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

TEST (yamlcpp, contract)
{
	OPEN_PLUGIN ("system/elektra/modules/yamlcpp", "file/path");

	CppKeySet keys;
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbGet` failed");

	CLOSE_PLUGIN ();
}

void update_parent (CppKeySet expected, string const filepath)
{
	// We replace the value of the parent key of expected keyset, if the header file specifies the value @CONFIG_FILEPATH@.
	// We could also do that via CMake, but the current solution should be easier for now.
	CppKey root = expected.lookup (PREFIX, KDB_O_POP);
	if (root)
	{
		if (root.getString () == "@CONFIG_FILEPATH@") root.setString (filepath);
		expected.append (root);
	}
}

static void test_read (string const & filename, CppKeySet expected, int const status = ELEKTRA_PLUGIN_STATUS_SUCCESS)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress")))
#endif
{
	string filepath = srcdir_file (filename.c_str ());
	update_parent (expected, filepath);

	OPEN_PLUGIN (PREFIX, filepath.c_str ());

	CppKeySet keys;
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), status, "Call of `kdbGet` failed");
	compare_keyset (keys, expected);

	CLOSE_PLUGIN ();
}

static void test_write_read (CppKeySet expected)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress")))
#endif
{
	string filepath = elektraFilename ();
	update_parent (expected, filepath);

	OPEN_PLUGIN (PREFIX, filepath.c_str ());

	// Write key set to file
	succeed_if_same (plugin->kdbSet (plugin, expected.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS, "Unable to write to file");

	// Read written data
	CppKeySet keySetRead;
	succeed_if_same (plugin->kdbGet (plugin, keySetRead.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Unable to open or parse file");

	// Compare data
	compare_keyset (keySetRead, expected);

	// Clean up
	CLOSE_PLUGIN ();
}

TEST (yamlcpp, flat)
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

TEST (yamlcpp, nested)
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

TEST (yamlcpp, array)
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
	test_write_read (
#include "yamlcpp/mapping_with_array_key.h"
	);
	test_write_read (
#include "yamlcpp/mixed.h"
	);
}

// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char * argv[])
{
	init (argc, argv); // Required for `srcdir_file` to work properly
	::testing::InitGoogleTest (&argc, argv);
	return RUN_ALL_TESTS ();
}
