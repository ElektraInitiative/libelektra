/**
 * @file
 *
 * @brief Source for cpptemplate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "cpptemplate.hpp"
#include "cpptemplate_delegate.hpp"

#include <kdberrors.h>
#include <kdbhelper.h>

using ckdb::keyNew;
using std::exception;

using elektra::CppTemplateDelegate;

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
			  keyNew ("system:/elektra/modules/cpptemplate", ELEKTRA_KEY_VALUE, "cpptemplate plugin waits for your orders", ELEKTRA_KEY_END),
			  keyNew ("system:/elektra/modules/cpptemplate/exports", ELEKTRA_KEY_END),
			  keyNew ("system:/elektra/modules/cpptemplate/exports/open", ELEKTRA_KEY_FUNC, elektraCppTemplateOpen, ELEKTRA_KEY_END),
			  keyNew ("system:/elektra/modules/cpptemplate/exports/close", ELEKTRA_KEY_FUNC, elektraCppTemplateClose, ELEKTRA_KEY_END),
			  keyNew ("system:/elektra/modules/cpptemplate/exports/get", ELEKTRA_KEY_FUNC, elektraCppTemplateGet, ELEKTRA_KEY_END),
			  keyNew ("system:/elektra/modules/cpptemplate/exports/set", ELEKTRA_KEY_FUNC, elektraCppTemplateSet, ELEKTRA_KEY_END),
			  keyNew ("system:/elektra/modules/cpptemplate/exports/error", ELEKTRA_KEY_FUNC, elektraCppTemplateError, ELEKTRA_KEY_END),
			  keyNew ("system:/elektra/modules/cpptemplate/exports/checkconf", ELEKTRA_KEY_FUNC, elektraCppTemplateCheckConf, ELEKTRA_KEY_END),
#include ELEKTRA_README
			  keyNew ("system:/elektra/modules/cpptemplate/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END),
			  ELEKTRA_KS_END };
}

} // end namespace

extern "C" {

typedef Delegator<CppTemplateDelegate> delegator;

/** @see elektraDocOpen */
int elektraCppTemplateOpen (Plugin * handle, Key * key)
{
	int status = ELEKTRA_PLUGIN_STATUS_ERROR;

	try
	{
		// - The function below calls the constructor `CppTemplateDelegate(config)`.
		// - After the call to `delegator::open` you can retrieve a pointer to the delegate via `delegator::get (handle)`.
		status = delegator::open (handle, key);
	}
	catch (exception const & error)
	{
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (key, "Uncaught Exception: %s", error.what ());
	}

	return status;
}

/** @see elektraDocClose */
int elektraCppTemplateClose (Plugin * handle, Key * key)
{
	// The function `delegator::close` calls the destructor of `CppTemplateDelegate`.
	return delegator::close (handle, key);
}

/** @see elektraDocGet */
int elektraCppTemplateGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	if (parent.getName () == "system:/elektra/modules/cpptemplate")
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
int elektraCppTemplateSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

/** @see elektraDocError */
int elektraCppTemplateError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/** @see elektraDocCheckConf */
int elektraCppTemplateCheckConf (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("cpptemplate",
		ELEKTRA_PLUGIN_OPEN,	&elektraCppTemplateOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraCppTemplateClose,
		ELEKTRA_PLUGIN_GET,	&elektraCppTemplateGet,
		ELEKTRA_PLUGIN_SET,	&elektraCppTemplateSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraCppTemplateError,
		ELEKTRA_PLUGIN_END);
}

} // end extern "C"
