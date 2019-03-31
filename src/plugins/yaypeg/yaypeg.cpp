/**
 * @file
 *
 * @brief Source for yaypeg plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "yaypeg.hpp"

#include <stdexcept>

#include <kdb.hpp>
#include <kdberrors.h>
#include <kdbhelper.h>

#include "convert.hpp"

using std::exception;
using std::runtime_error;

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;

using yaypeg::addToKeySet;

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
			  keyNew ("system/elektra/modules/yaypeg", KEY_VALUE, "yaypeg plugin waits for your orders", KEY_END),
			  keyNew ("system/elektra/modules/yaypeg/exports", KEY_END),
			  keyNew ("system/elektra/modules/yaypeg/exports/get", KEY_FUNC, elektraYaypegGet, KEY_END),
#include ELEKTRA_README
			  keyNew ("system/elektra/modules/yaypeg/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  KS_END };
}

} // end namespace

extern "C" {

/** @see elektraDocGet */
int elektraYaypegGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	if (parent.getName () == "system/elektra/modules/yaypeg")
	{
		keys.append (getContract ());
		parent.release ();
		keys.release ();
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	auto status = ELEKTRA_PLUGIN_STATUS_ERROR;
	try
	{
		status = addToKeySet (keys, parent, parent.getString ());
	}
	catch (runtime_error const & runtimeError)
	{
		ELEKTRA_SET_ERROR (PARSING_CODE, *parent, runtimeError.what ());
	}
	catch (exception const & error)
	{
		ELEKTRA_SET_ERRORF (INSTALLATION_CODE, *parent, "Uncaught exception: %s", error.what ());
	}

	parent.release ();
	keys.release ();
	return status < 0 ? ELEKTRA_PLUGIN_STATUS_ERROR : status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("yaypeg", ELEKTRA_PLUGIN_GET, &elektraYaypegGet, ELEKTRA_PLUGIN_END);
}

} // end extern "C"
