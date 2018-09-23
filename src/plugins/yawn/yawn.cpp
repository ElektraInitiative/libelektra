/**
 * @file
 *
 * @brief Source for yawn plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "yawn.hpp"
#include "yawn_delegate.hpp"

#include <kdberrors.h>
#include <kdbhelper.h>

using std::exception;

using elektra::YawnDelegate;

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
			  keyNew ("system/elektra/modules/yawn/exports/open", KEY_FUNC, elektraYawnOpen, KEY_END),
			  keyNew ("system/elektra/modules/yawn/exports/close", KEY_FUNC, elektraYawnClose, KEY_END),
			  keyNew ("system/elektra/modules/yawn/exports/get", KEY_FUNC, elektraYawnGet, KEY_END),
			  keyNew ("system/elektra/modules/yawn/exports/set", KEY_FUNC, elektraYawnSet, KEY_END),
			  keyNew ("system/elektra/modules/yawn/exports/error", KEY_FUNC, elektraYawnError, KEY_END),
			  keyNew ("system/elektra/modules/yawn/exports/checkconf", KEY_FUNC, elektraYawnCheckConfig, KEY_END),
#include ELEKTRA_README (yawn)
			  keyNew ("system/elektra/modules/yawn/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  KS_END };
}

} // end namespace

extern "C" {

typedef Delegator<YawnDelegate> delegator;

/** @see elektraDocOpen */
int elektraYawnOpen (Plugin * handle, Key * key)
{
	int status = ELEKTRA_PLUGIN_STATUS_ERROR;

	try
	{
		// - The function below calls the constructor `YawnDelegate(config)`.
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
int elektraYawnClose (Plugin * handle, Key * key)
{
	// The function `delegator::close` calls the destructor of `YawnDelegate`.
	return delegator::close (handle, key);
}

/** @see elektraDocGet */
int elektraYawnGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	if (parent.getName () == "system/elektra/modules/yawn")
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
int elektraYawnSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

/** @see elektraDocError */
int elektraYawnError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/** @see elektraDocCheckConf */
int elektraYawnCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yawn)
{
	// clang-format off
	return elektraPluginExport ("yawn",
		ELEKTRA_PLUGIN_OPEN,	&elektraYawnOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraYawnClose,
		ELEKTRA_PLUGIN_GET,	&elektraYawnGet,
		ELEKTRA_PLUGIN_SET,	&elektraYawnSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraYawnError,
		ELEKTRA_PLUGIN_END);
}

} // end extern "C"
