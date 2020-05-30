/**
 * @file
 *
 * @brief Tests for yanlr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include "yanlr.hpp"

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
	elektraModulesInit (modules.getKeySet (), 0);                                                                                      \
	CppKeySet config{ 0, KS_END };                                                                                                     \
	CppKey parent{ parentName, KEY_VALUE, filepath, KEY_END };                                                                         \
	Plugin * plugin = elektraPluginOpen ("yanlr", modules.getKeySet (), config.getKeySet (), *parent);

#define CLOSE_PLUGIN()                                                                                                                     \
	config.release ();                                                                                                                 \
	elektraPluginClose (plugin, 0);                                                                                                    \
	elektraModulesClose (modules.getKeySet (), 0)

#define PREFIX "user:/tests/yanlr/"

// -- Functions  ---------------------------------------------------------------------------------------------------------------------------

void test_read (string const & filepath, CppKeySet expected)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]"), annotate ("oclint:suppress[empty if statement]"),
			annotate ("oclint:suppress[too few branches in switch statement]")))
#endif
{
	OPEN_PLUGIN (PREFIX, srcdir_file (filepath.c_str ()));

	// We replace the value of the parent key of expected keyset, if the header file specifies the value @CONFIG_FILEPATH@.
	// We could also do that via CMake, but the current solution should be easier for now.
	CppKey root = expected.lookup (PREFIX, KDB_O_POP);
	if (root)
	{
		if (root.getString () == "@CONFIG_FILEPATH@") root.setString (srcdir_file (filepath.c_str ()));
		expected.append (root);
	}

	CppKeySet keys;
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbGet` failed");
	compare_keyset (expected, keys);

	CLOSE_PLUGIN ();
}

// -- Tests --------------------------------------------------------------------------------------------------------------------------------

TEST (yanlr, basics)
{
	OPEN_PLUGIN ("system:/elektra/modules/yanlr", "file/path");

	CppKeySet keys;
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Could not retrieve plugin contract");

	CLOSE_PLUGIN ();
}

TEST (yanlr, empty)
{
	test_read ("yanlr/null.yaml",
#include "yanlr/null.hpp"
	);
	test_read ("yanlr/comment.yaml",
#include "yanlr/null.hpp"
	);
}

TEST (yanlr, scalar)
{
	test_read ("yanlr/plain_scalar-word_chars.yaml",
#include "yanlr/plain_scalar-word_chars.hpp"
	);
	test_read ("yanlr/plain_scalar-word_chars_space.yaml",
#include "yanlr/plain_scalar-word_chars_space.hpp"
	);
	test_read ("yanlr/single_quoted_scalar.yaml",
#include "yanlr/single_quoted_scalar.hpp"
	);
	test_read ("yanlr/double_quoted_scalar.yaml",
#include "yanlr/double_quoted_scalar.hpp"
	);
}

TEST (yanlr, list)
{
	test_read ("yanlr/list-plain_scalars.yaml",
#include "yanlr/list-plain_scalars.hpp"
	);
	test_read ("yanlr/list-list_map-mixed_scalars.yaml",
#include "yanlr/list-list_map-mixed_scalars.hpp"
	);
}

TEST (yanlr, map)
{
	test_read ("yanlr/map-null.yaml",
#include "yanlr/map-null.hpp"
	);
	test_read ("yanlr/map-plain_scalar.yaml",
#include "yanlr/map-plain_scalar.hpp"
	);
	test_read ("yanlr/map-plain_scalars.yaml",
#include "yanlr/map-plain_scalars.hpp"
	);
	test_read ("yanlr/map-list-plain_scalars.yaml",
#include "yanlr/map-list-plain_scalars.hpp"
	);
	test_read ("yanlr/map-map-plain_scalars.yaml",
#include "yanlr/map-map-plain_scalars.hpp"
	);
}

// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char * argv[])
{
	init (argc, argv); // Required for `srcdir_file` to work properly
	::testing::InitGoogleTest (&argc, argv);
	return RUN_ALL_TESTS ();
}
