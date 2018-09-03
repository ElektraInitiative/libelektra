/**
 * @file
 *
 * @brief Source for yambi plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdb.hpp>
#include <kdbconfig.h>

#include "yambi.hpp"

using ckdb::keyNew;

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
			  keyNew ("system/elektra/modules/yambi", KEY_VALUE, "yambi plugin waits for your orders", KEY_END),
			  keyNew ("system/elektra/modules/yambi/exports", KEY_END),
			  keyNew ("system/elektra/modules/yambi/exports/get", KEY_FUNC, elektraYambiGet, KEY_END),
			  keyNew ("system/elektra/modules/yambi/exports/set", KEY_FUNC, elektraYambiSet, KEY_END),
#include ELEKTRA_README (yambi)
			  keyNew ("system/elektra/modules/yambi/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  KS_END };
}

} // end namespace

extern "C" {

/** @see elektraDocGet */
int elektraYambiGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	if (parent.getName () == "system/elektra/modules/yambi")
	{
		keys.append (getContract ());
	}

	parent.release ();
	keys.release ();
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/** @see elektraDocSet */
int elektraYambiSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yambi)
{
	return elektraPluginExport ("yambi", ELEKTRA_PLUGIN_GET, &elektraYambiGet, ELEKTRA_PLUGIN_SET, &elektraYambiSet,
				    ELEKTRA_PLUGIN_END);
}

} // end extern "C"
