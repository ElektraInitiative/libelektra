/**
 * @file
 *
 * @brief Source for yaypeg plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "yaypeg.hpp"
#include "yaypeg_delegate.hpp"

#include <kdberrors.h>
#include <kdbhelper.h>

using std::exception;

using elektra::YaypegDelegate;

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
			  keyNew ("system/elektra/modules/yaypeg", KEY_VALUE, "yaypeg plugin waits for your orders", KEY_END),
			  keyNew ("system/elektra/modules/yaypeg/exports", KEY_END),
			  keyNew ("system/elektra/modules/yaypeg/exports/open", KEY_FUNC, elektraYaypegOpen, KEY_END),
			  keyNew ("system/elektra/modules/yaypeg/exports/close", KEY_FUNC, elektraYaypegClose, KEY_END),
			  keyNew ("system/elektra/modules/yaypeg/exports/get", KEY_FUNC, elektraYaypegGet, KEY_END),
			  keyNew ("system/elektra/modules/yaypeg/exports/set", KEY_FUNC, elektraYaypegSet, KEY_END),
			  keyNew ("system/elektra/modules/yaypeg/exports/error", KEY_FUNC, elektraYaypegError, KEY_END),
			  keyNew ("system/elektra/modules/yaypeg/exports/checkconf", KEY_FUNC, elektraYaypegCheckConfig, KEY_END),
#include ELEKTRA_README (yaypeg)
			  keyNew ("system/elektra/modules/yaypeg/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  KS_END };
}

} // end namespace

extern "C" {

typedef Delegator<YaypegDelegate> delegator;

/** @see elektraDocOpen */
int elektraYaypegOpen (Plugin * handle, Key * key)
{
	int status = ELEKTRA_PLUGIN_STATUS_ERROR;

	try
	{
		// - The function below calls the constructor `YaypegDelegate(config)`.
		// - After the call to `delegator::open` you can retrieve a pointer to the delegate via `delegator::get (handle)`.
		status = delegator::open (handle, key);
	}
	catch (exception const & error)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_UNCAUGHT_EXCEPTION, key, error.what ());
	}

	return status;
}

/** @see elektraDocClose */
int elektraYaypegClose (Plugin * handle, Key * key)
{
	// The function `delegator::close` calls the destructor of `YaypegDelegate`.
	return delegator::close (handle, key);
}

/** @see elektraDocGet */
int elektraYaypegGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	if (parent.getName () == "system/elektra/modules/yaypeg")
	{
		keys.append (getContract ());
	}
	else
	{
		// This is only an example, to show you how to call a method of the delegate
		keys.append (delegator::get (handle)->getConfig (parent));
	}

	parent.release ();
	keys.release ();
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/** @see elektraDocSet */
int elektraYaypegSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

/** @see elektraDocError */
int elektraYaypegError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/** @see elektraDocCheckConf */
int elektraYaypegCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yaypeg)
{
	// clang-format off
	return elektraPluginExport ("yaypeg",
		ELEKTRA_PLUGIN_OPEN,	&elektraYaypegOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraYaypegClose,
		ELEKTRA_PLUGIN_GET,	&elektraYaypegGet,
		ELEKTRA_PLUGIN_SET,	&elektraYaypegSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraYaypegError,
		ELEKTRA_PLUGIN_END);
}

} // end extern "C"
