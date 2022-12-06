/**
 * @file
 *
 * @brief Source for kconfig plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "kconfig.hpp"
#include "kconfig_delegate.hpp"
#include "kconfig_serializer.hpp"

#include <fstream>
#include <kdberrors.h>
#include <elektra/kdbhelper.h>

using ckdb::keyNew;
using std::exception;

using elektra::KconfigDelegate;

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
			  keyNew ("system:/elektra/modules/kconfig", KEY_VALUE, "kconfig plugin waits for your orders", KEY_END),
			  keyNew ("system:/elektra/modules/kconfig/exports", KEY_END),
			  keyNew ("system:/elektra/modules/kconfig/exports/open", KEY_FUNC, elektraKconfigOpen, KEY_END),
			  keyNew ("system:/elektra/modules/kconfig/exports/close", KEY_FUNC, elektraKconfigClose, KEY_END),
			  keyNew ("system:/elektra/modules/kconfig/exports/get", KEY_FUNC, elektraKconfigGet, KEY_END),
			  keyNew ("system:/elektra/modules/kconfig/exports/set", KEY_FUNC, elektraKconfigSet, KEY_END),
			  keyNew ("system:/elektra/modules/kconfig/exports/error", KEY_FUNC, elektraKconfigError, KEY_END),
			  keyNew ("system:/elektra/modules/kconfig/exports/checkconf", KEY_FUNC, elektraKconfigCheckConf, KEY_END),
#include ELEKTRA_README
			  keyNew ("system:/elektra/modules/kconfig/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  KS_END };
}

} // end namespace

extern "C" {

typedef Delegator<KconfigDelegate> delegator;

/** @see elektraDocOpen */
int elektraKconfigOpen (Plugin * handle, Key * key)
{
	int status = ELEKTRA_PLUGIN_STATUS_ERROR;

	try
	{
		// - The function below calls the constructor `KconfigDelegate(config)`.
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
int elektraKconfigClose (Plugin * handle, Key * key)
{
	// The function `delegator::close` calls the destructor of `KconfigDelegate`.
	return delegator::close (handle, key);
}

/** @see elektraDocGet */
int elektraKconfigGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	if (parent.getName () == "system:/elektra/modules/kconfig")
	{
		keys.append (getContract ());
		parent.release ();
		keys.release ();
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}


	int status = ELEKTRA_PLUGIN_STATUS_ERROR;

	try
	{
		keys.append (delegator::get (handle)->getConfig (parent));
		status = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	catch (std::overflow_error const & exception)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parent.getKey (), "Unable to read data from file '%s'. Reason: %s",
					     parent.getString ().c_str (), exception.what ());
	}
	catch (std::runtime_error const & exception)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parent.getKey (), "Unable to parse file '%s'. Reason: %s",
							 parent.getString ().c_str (), exception.what ());
	}

	parent.release ();
	keys.release ();

	return status;
}

/** @see elektraDocSet */
int elektraKconfigSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	ELEKTRA_LOG_DEBUG ("Save `%s` using the kconfig plugin", parent.getName ().c_str ());
	auto filePtr = new std::ofstream{ parent.getString () };
	bool isFileOpen = filePtr->is_open ();
	std::unique_ptr<std::ostream> file{ filePtr };

	if (!isFileOpen)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parent.getKey (), "Unable to save data to file '%s'. Reason: %s", parent.getString ().c_str (),
					     "Could not open the file.");
		parent.release ();
		keys.release ();
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	ELEKTRA_LOG_DEBUG ("File opened successfully, start saving the data.");

	KConfigSerializer serializer{ keys, parent, std::move (file) };
	serializer.save ();

	ELEKTRA_LOG_DEBUG ("Data succesfully stored into `%s`.", parent.getName ().c_str ());

	parent.release ();
	keys.release ();
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/** @see elektraDocError */
int elektraKconfigError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/** @see elektraDocCheckConf */
int elektraKconfigCheckConf (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("kconfig",
		ELEKTRA_PLUGIN_OPEN,	&elektraKconfigOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraKconfigClose,
		ELEKTRA_PLUGIN_GET,	&elektraKconfigGet,
		ELEKTRA_PLUGIN_SET,	&elektraKconfigSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraKconfigError,
		ELEKTRA_PLUGIN_END);
}

} // end extern "C"
