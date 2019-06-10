/**
 * @file
 *
 * @brief Tests for yawn plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include "yawn.hpp"

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
	Plugin * plugin = elektraPluginOpen ("yawn", modules.getKeySet (), config.getKeySet (), *parent);                                  \
	exit_if_fail (plugin != NULL, "Could not open yawn plugin")

#define CLOSE_PLUGIN()                                                                                                                     \
	elektraPluginClose (plugin, 0);                                                                                                    \
	elektraModulesClose (modules.getKeySet (), 0);                                                                                     \
	ksDel (modules.release ());                                                                                                        \
	config.release ()

#define PREFIX "user/tests/yawn/"

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

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

TEST (yawn, basics)
{
	OPEN_PLUGIN ("system/elektra/modules/yawn", "file/path");

	CppKeySet keys;
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbGet` failed");

	CLOSE_PLUGIN ();
}


TEST (yawn, empty)
{
	test_read ("yawn/null.yaml",
#include "yawn/null.hpp"
	);
	test_read ("yawn/comment.yaml",
#include "yawn/null.hpp"
	);
}

TEST (yawn, scalar)
{
	test_read ("yawn/plain_scalar-word_chars.yaml",
#include "yawn/plain_scalar-word_chars.hpp"
	);
	test_read ("yawn/plain_scalar-word_chars_space.yaml",
#include "yawn/plain_scalar-word_chars_space.hpp"
	);
	test_read ("yawn/single_quoted_scalar.yaml",
#include "yawn/single_quoted_scalar.hpp"
	);
	test_read ("yawn/double_quoted_scalar.yaml",
#include "yawn/double_quoted_scalar.hpp"
	);
}

TEST (yawn, list)
{
	test_read ("yawn/list-plain_scalars.yaml",
#include "yawn/list-plain_scalars.hpp"
	);
	test_read ("yawn/list-list_map-mixed_scalars.yaml",
#include "yawn/list-list_map-mixed_scalars.hpp"
	);
}

TEST (yawn, map)
{
	test_read ("yawn/map-null.yaml",
#include "yawn/map-null.hpp"
	);
	test_read ("yawn/map-plain_scalar.yaml",
#include "yawn/map-plain_scalar.hpp"
	);
	test_read ("yawn/map-plain_scalars.yaml",
#include "yawn/map-plain_scalars.hpp"
	);
	test_read ("yawn/map-list-plain_scalars.yaml",
#include "yawn/map-list-plain_scalars.hpp"
	);
	test_read ("yawn/map-map-plain_scalars.yaml",
#include "yawn/map-map-plain_scalars.hpp"
	);
}

// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char * argv[])
{
	init (argc, argv); // Required for `srcdir_file` to work properly
	::testing::InitGoogleTest (&argc, argv);
	return RUN_ALL_TESTS ();
}
