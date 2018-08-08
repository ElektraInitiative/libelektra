/**
 * @file
 *
 * @brief Tests for leaf plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "leaf.hpp"

#include <kdbmodule.h>
#include <kdbprivate.h>

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
	Plugin * plugin = elektraPluginOpen ("leaf", modules.getKeySet (), config.getKeySet (), *parent);                                  \
	exit_if_fail (plugin != NULL, "Could not open leaf plugin");

#define CLOSE_PLUGIN()                                                                                                                     \
	ksDel (modules.release ());                                                                                                        \
	config.release ();                                                                                                                 \
	elektraPluginClose (plugin, 0);                                                                                                    \
	elektraModulesClose (modules.getKeySet (), 0)

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

void test_set (CppKeySet keys, CppKeySet expected, int const status = ELEKTRA_PLUGIN_STATUS_SUCCESS)
{
	OPEN_PLUGIN ("user/tests/leaf", "file/path"); //! OCLint (too few branches switch, empty if statement)

	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), *parent), //! OCLint (too few branches switch, empty if statement)
			 status, "Call of `kdbSet` failed");

	compare_keyset (keys, expected); //! OCLint (too few branches switch)

	CLOSE_PLUGIN ();
}

// -- Tests --------------------------------------------------------------------------------------------------------------------------------

TEST (leaf, basics)
{
	OPEN_PLUGIN ("system/elektra/modules/leaf", "")

	CppKeySet keys{ 0, KS_END };
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Unable to retrieve plugin contract");

	CLOSE_PLUGIN ();
}

TEST (leaf, set)
{
	test_set (
#include "leaf/empty.hpp"
		,
#include "leaf/empty.hpp"
		, ELEKTRA_PLUGIN_STATUS_NO_UPDATE);

	test_set (
#include "leaf/simple_get.hpp"
		,
#include "leaf/simple_set.hpp"
	);
}
