/**
 * @file
 *
 * @brief Source for yawn plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <exception>

#include <kdb.hpp>
#include <kdberrors.h>
#include <kdbhelper.h>

#include "yawn.hpp"

using std::exception;

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
			  keyNew ("system/elektra/modules/yawn", KEY_VALUE, "yawn plugin waits for your orders", KEY_END),
			  keyNew ("system/elektra/modules/yawn/exports", KEY_END),
			  keyNew ("system/elektra/modules/yawn/exports/get", KEY_FUNC, elektraYawnGet, KEY_END),
			  keyNew ("system/elektra/modules/yawn/exports/set", KEY_FUNC, elektraYawnSet, KEY_END),
#include ELEKTRA_README (yawn)
			  keyNew ("system/elektra/modules/yawn/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  KS_END };
}

} // end namespace

extern "C" {

/** @see elektraDocGet */
int elektraYawnGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	if (parent.getName () == "system/elektra/modules/yawn")
	{
		keys.append (getContract ());
	}

	parent.release ();
	keys.release ();
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/** @see elektraDocSet */
int elektraYawnSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yawn)
{
	return elektraPluginExport ("yawn", ELEKTRA_PLUGIN_GET, &elektraYawnGet, ELEKTRA_PLUGIN_SET, &elektraYawnSet, ELEKTRA_PLUGIN_END);
}

} // end extern "C"
