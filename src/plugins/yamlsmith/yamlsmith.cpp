/**
 * @file
 *
 * @brief Source for yamlsmith plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include "yamlsmith.hpp"

#include <kdb.hpp>
#include <kdbhelper.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::keyNew;

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

namespace
{

/**
 * @brief This function returns a key set containing the contract of the plugin.
 *
 * @return A contract describing the functionality of this plugin
 */
CppKeySet contractYamlsmith ()
{
	return CppKeySet (30, keyNew ("system/elektra/modules/yamlsmith", KEY_VALUE, "yamlsmith plugin waits for your orders", KEY_END),
			  keyNew ("system/elektra/modules/yamlsmith/exports", KEY_END),
			  keyNew ("system/elektra/modules/yamlsmith/exports/get", KEY_FUNC, elektraYamlsmithGet, KEY_END),
			  keyNew ("system/elektra/modules/yamlsmith/exports/set", KEY_FUNC, elektraYamlsmithSet, KEY_END),
#include ELEKTRA_README (yamlsmith)
			  keyNew ("system/elektra/modules/yamlsmith/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}
} // end namespace

extern "C" {
// ====================
// = Plugin Interface =
// ====================

/** @see elektraDocGet */
int elektraYamlsmithGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	auto parent = CppKey{ parentKey };
	auto keys = CppKeySet{ returned };

	if (parent.getName () == "system/elektra/modules/yamlsmith")
	{
		keys.append (contractYamlsmith ());
		parent.release ();
		keys.release ();

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	parent.release ();

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

/** @see elektraDocSet */
int elektraYamlsmithSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yamlsmith)
{
	return elektraPluginExport ("yamlsmith", ELEKTRA_PLUGIN_GET, &elektraYamlsmithGet, ELEKTRA_PLUGIN_SET, &elektraYamlsmithSet,
				    ELEKTRA_PLUGIN_END);
}

} // end extern "C"
