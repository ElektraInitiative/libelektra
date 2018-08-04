/**
 * @file
 *
 * @brief Source for leaf plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "leaf.hpp"
#include "leaf_delegate.hpp"

#include <kdbhelper.h>

using elektra::LeafDelegate;

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;

namespace
{

/**
 * @brief This function returns a key set containing the plugin contract.
 *
 * @return A key set specifying the capabilities of the plugin
 */
CppKeySet getContract ()
{
	return CppKeySet{ 30,
			  keyNew ("system/elektra/modules/leaf", KEY_VALUE, "leaf plugin waits for your orders", KEY_END),
			  keyNew ("system/elektra/modules/leaf/exports", KEY_END),
			  keyNew ("system/elektra/modules/leaf/exports/get", KEY_FUNC, elektraLeafGet, KEY_END),
			  keyNew ("system/elektra/modules/leaf/exports/set", KEY_FUNC, elektraLeafSet, KEY_END),
#include ELEKTRA_README (leaf)
			  keyNew ("system/elektra/modules/leaf/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  KS_END };
}

} // end namespace

extern "C" {

typedef Delegator<LeafDelegate> delegator;

/** @see elektraDocGet */
int elektraLeafGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };
	bool updated = false;

	if (parent.getName () == "system/elektra/modules/leaf")
	{
		keys.append (getContract ());
		updated = true;
	}

	parent.release ();
	keys.release ();
	return updated ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

/** @see elektraDocSet */
int elektraLeafSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (leaf)
{
	return elektraPluginExport ("leaf", ELEKTRA_PLUGIN_GET, &elektraLeafGet, ELEKTRA_PLUGIN_SET, &elektraLeafSet, ELEKTRA_PLUGIN_END);
}

} // end extern "C"
