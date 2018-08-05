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

// -- Tests --------------------------------------------------------------------------------------------------------------------------------

TEST (leaf, basics)
{
	OPEN_PLUGIN ("system/elektra/modules/leaf", "")

	CppKeySet keys{ 0, KS_END };
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Unable to retrieve plugin contract");
	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "Call of `kdbSet` failed");

	CLOSE_PLUGIN ();
}
