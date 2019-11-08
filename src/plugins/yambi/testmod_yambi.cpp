/**
 * @file
 *
 * @brief Tests for yambi plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "yambi.hpp"

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
	Plugin * plugin = elektraPluginOpen ("yambi", modules.getKeySet (), config.getKeySet (), *parent);

#define CLOSE_PLUGIN()                                                                                                                     \
	config.release ();                                                                                                                 \
	elektraPluginClose (plugin, 0);                                                                                                    \
	elektraModulesClose (modules.getKeySet (), 0)

#define PREFIX "user:/tests/yambi/"

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

TEST (yambi, basics)
{
	OPEN_PLUGIN ("system:/elektra/modules/yambi", "file/path");

	CppKeySet keys;
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbGet` failed");

	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "Call of `kdbSet` failed");

	CLOSE_PLUGIN ();
}


TEST (yambi, empty)
{
	test_read ("yambi/null.yaml",
#include "yambi/null.hpp"
	);
	test_read ("yambi/comment.yaml",
#include "yambi/null.hpp"
	);
}

TEST (yambi, scalar)
{
	test_read ("yambi/plain_scalar-word_chars.yaml",
#include "yambi/plain_scalar-word_chars.hpp"
	);
	test_read ("yambi/plain_scalar-word_chars_space.yaml",
#include "yambi/plain_scalar-word_chars_space.hpp"
	);
	test_read ("yambi/single_quoted_scalar.yaml",
#include "yambi/single_quoted_scalar.hpp"
	);
	test_read ("yambi/double_quoted_scalar.yaml",
#include "yambi/double_quoted_scalar.hpp"
	);
}

TEST (yambi, list)
{
	test_read ("yambi/list-plain_scalars.yaml",
#include "yambi/list-plain_scalars.hpp"
	);
	test_read ("yambi/list-list_map-mixed_scalars.yaml",
#include "yambi/list-list_map-mixed_scalars.hpp"
	);
}

TEST (yambi, map)
{
	test_read ("yambi/map-null.yaml",
#include "yambi/map-null.hpp"
	);
	test_read ("yambi/map-plain_scalar.yaml",
#include "yambi/map-plain_scalar.hpp"
	);
	test_read ("yambi/map-plain_scalars.yaml",
#include "yambi/map-plain_scalars.hpp"
	);
	test_read ("yambi/map-list-plain_scalars.yaml",
#include "yambi/map-list-plain_scalars.hpp"
	);
	test_read ("yambi/map-map-plain_scalars.yaml",
#include "yambi/map-map-plain_scalars.hpp"
	);
}

// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char * argv[])
{
	init (argc, argv); // Required for `srcdir_file` to work properly
	::testing::InitGoogleTest (&argc, argv);
	return RUN_ALL_TESTS ();
}
