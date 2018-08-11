/**
 * @file
 *
 * @brief Source for leaf plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "leaf.hpp"

#include <kdbhelper.h>

using std::exception;

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
			  keyNew ("system/elektra/modules/leaf/exports/open", KEY_FUNC, elektraLeafOpen, KEY_END),
			  keyNew ("system/elektra/modules/leaf/exports/close", KEY_FUNC, elektraLeafClose, KEY_END),
			  keyNew ("system/elektra/modules/leaf/exports/get", KEY_FUNC, elektraLeafGet, KEY_END),
			  keyNew ("system/elektra/modules/leaf/exports/set", KEY_FUNC, elektraLeafSet, KEY_END),
#include ELEKTRA_README (leaf)
			  keyNew ("system/elektra/modules/leaf/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  KS_END };
}

} // end namespace

extern "C" {

typedef Delegator<LeafDelegate> delegator;

/** @see elektraDocOpen */
int elektraLeafOpen (Plugin * handle, Key * key)
{
	// After the call to `delegator::open` you can retrieve a pointer to the delegate via `coderDelegator::get (handle)`
	return delegator::open (handle, key);
}

/** @see elektraDocClose */
int elektraLeafClose (Plugin * handle, Key * key)
{
	return delegator::close (handle, key);
}

/** @see elektraDocGet */
int elektraLeafGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	if (parent.getName () == "system/elektra/modules/leaf")
	{
		keys.append (getContract ());
		parent.release ();
		keys.release ();
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	int status = ELEKTRA_PLUGIN_STATUS_ERROR;
	try
	{
		status = delegator::get (handle)->convertToDirectories (keys);
	}
	catch (exception const & error)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_UNCAUGHT_EXCEPTION, *parent, error.what ());
	}

	parent.release ();
	keys.release ();
	return status;
}

/** @see elektraDocSet */
int elektraLeafSet (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	int status = ELEKTRA_PLUGIN_STATUS_ERROR;
	try
	{
		status = delegator::get (handle)->convertToLeaves (keys);
	}
	catch (exception const & error)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_UNCAUGHT_EXCEPTION, *parent, error.what ());
	}

	parent.release ();
	keys.release ();
	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (leaf)
{
	// clang-format off
	return elektraPluginExport ("leaf",
		ELEKTRA_PLUGIN_OPEN,	&elektraLeafOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraLeafClose,
		ELEKTRA_PLUGIN_GET,	&elektraLeafGet,
		ELEKTRA_PLUGIN_SET,	&elektraLeafSet,
		ELEKTRA_PLUGIN_END);
}

} // end extern "C"
