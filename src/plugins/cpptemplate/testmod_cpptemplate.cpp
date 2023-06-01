/**
 * @file
 *
 * @brief Tests for cpptemplate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./cpptemplate.hpp"

#include <internal/plugin/load.h>
#include <internal/plugin/struct.h>
#include <internal/pluginload/module.h>

#include <tests.hpp>

using CppKeySet = kdb::KeySet;
using CppKey = kdb::Key;

TEST (cpptemplate, basics) //! OCLint (avoid private static members)
#ifdef __llvm__
__attribute__ ((annotate ("oclint:suppress[empty if statement]"), annotate ("oclint:suppress[high ncss method]"),
		annotate ("oclint:suppress[too few branches in switch statement]")))
#endif
{
	CppKeySet modules{ 0, KS_END };
	CppKeySet config{ 0, KS_END };
	CppKeySet keys{ 0, KS_END };
	elektraModulesInit (modules.getKeySet (), 0);

	CppKey parent{ "system:/elektra/modules/cpptemplate", KEY_END };
	Plugin * plugin = elektraPluginOpen ("cpptemplate", modules.getKeySet (), config.getKeySet (), *parent);
	exit_if_fail (plugin != NULL, "Could not open cpptemplate plugin"); //! OCLint (empty if, too few branches switch)

	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbGet` failed");

	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "Call of `kdbSet` failed");

	succeed_if_same (plugin->kdbError (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbError` failed");

	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules.getKeySet (), 0);

	ksDel (modules.release ());
	config.release ();
}
