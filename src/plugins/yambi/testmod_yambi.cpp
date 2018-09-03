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

#include <tests.hpp>

using CppKeySet = kdb::KeySet;
using CppKey = kdb::Key;

TEST (yambi, basics)
{
	CppKeySet modules{ 0, KS_END };
	CppKeySet config{ 0, KS_END };
	CppKeySet keys{ 0, KS_END };
	elektraModulesInit (modules.getKeySet (), 0);

	CppKey parent{ "system/elektra/modules/yambi", KEY_END };
	Plugin * plugin = elektraPluginOpen ("yambi", modules.getKeySet (), config.getKeySet (), *parent);
	exit_if_fail (plugin != NULL, "Could not open yambi plugin"); //! OCLint (empty if, too few branches switch)

	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbGet` failed");

	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "Call of `kdbSet` failed");

	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules.getKeySet (), 0);

	ksDel (modules.release ());
	config.release ();
}
