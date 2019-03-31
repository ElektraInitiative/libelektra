/**
 * @file
 *
 * @brief Source for yawn plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <exception>

#include <kdb.hpp>
#include <kdberrors.h>
#include <kdbhelper.h>

#include "convert.hpp"
#include "yawn.hpp"

using std::exception;
using std::runtime_error;

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

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
#include ELEKTRA_README
			  keyNew ("system/elektra/modules/yawn/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  KS_END };
}

} // end namespace

// ====================
// = Plugin Interface =
// ====================

extern "C" {

/** @see elektraDocGet */
int elektraYawnGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	using yawn::addToKeySet;

	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	if (parent.getName () == "system/elektra/modules/yawn")
	{
		keys.append (getContract ());
		parent.release ();
		keys.release ();
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	int status = ELEKTRA_PLUGIN_STATUS_ERROR;
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
	return elektraPluginExport ("yawn", ELEKTRA_PLUGIN_GET, &elektraYawnGet, ELEKTRA_PLUGIN_END);
}

} // end extern "C"
