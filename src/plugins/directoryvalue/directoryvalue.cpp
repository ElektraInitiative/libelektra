/**
 * @file
 *
 * @brief Source for directoryvalue plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "directoryvalue.hpp"

#include <kdbhelper.h>

using std::exception;
using std::range_error;

using elektra::DirectoryValueDelegate;

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
			  keyNew ("system/elektra/modules/directoryvalue", KEY_VALUE, "directoryvalue plugin waits for your orders",
				  KEY_END),
			  keyNew ("system/elektra/modules/directoryvalue/exports", KEY_END),
			  keyNew ("system/elektra/modules/directoryvalue/exports/open", KEY_FUNC, elektraDirectoryValueOpen, KEY_END),
			  keyNew ("system/elektra/modules/directoryvalue/exports/close", KEY_FUNC, elektraDirectoryValueClose, KEY_END),
			  keyNew ("system/elektra/modules/directoryvalue/exports/get", KEY_FUNC, elektraDirectoryValueGet, KEY_END),
			  keyNew ("system/elektra/modules/directoryvalue/exports/set", KEY_FUNC, elektraDirectoryValueSet, KEY_END),
#include ELEKTRA_README
			  keyNew ("system/elektra/modules/directoryvalue/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  KS_END };
}

} // end namespace

extern "C" {

typedef Delegator<DirectoryValueDelegate> delegator;

/** @see elektraDocOpen */
int elektraDirectoryValueOpen (Plugin * handle, Key * key)
{
	return delegator::open (handle, key);
}

/** @see elektraDocClose */
int elektraDirectoryValueClose (Plugin * handle, Key * key)
{
	return delegator::close (handle, key);
}

/** @see elektraDocGet */
int elektraDirectoryValueGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	if (parent.getName () == "system/elektra/modules/directoryvalue")
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
	catch (range_error const & error)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (*parent, "Unable to insert array value %s", error.what ());
	}
	catch (exception const & error)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (*parent, "Uncaught Exception: %s", error.what ());
	}

#ifdef HAVE_LOGGER
	for (auto key : keys)
	{
		ELEKTRA_LOG_DEBUG ("\t“%s”: “%s”", key.getName ().c_str (),
				   key.getBinarySize () == 0 ? "NULL" : key.isBinary () ? "binary value!" : key.getString ().c_str ());
	}
#endif

	parent.release ();
	keys.release ();
	return status;
}

/** @see elektraDocSet */
int elektraDirectoryValueSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	int status = ELEKTRA_PLUGIN_STATUS_ERROR;
	try
	{
		status = delegator::get (handle)->convertToLeaves (keys);
	}
	catch (range_error const & error)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (*parent, "Unable to insert array value %s", error.what ());
	}
	catch (exception const & error)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (*parent, "Uncaught exception: %s", error.what ());
	}

	parent.release ();
	keys.release ();
	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("directoryvalue",
		ELEKTRA_PLUGIN_OPEN,	&elektraDirectoryValueOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDirectoryValueClose,
		ELEKTRA_PLUGIN_GET,	&elektraDirectoryValueGet,
		ELEKTRA_PLUGIN_SET,	&elektraDirectoryValueSet,
		ELEKTRA_PLUGIN_END);
}

} // end extern "C"
