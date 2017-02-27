/**
 * @file this file contains the entry point to the plugin as a gateway between c and c++
 *
 * @brief Plugin enables storage to xml files via the Xerces library
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "xerces.hpp"
#include "deserializer.hpp"
#include <stdio.h>

#include <kdbhelper.h>
#include <xercesc/util/PlatformUtils.hpp>

using namespace ckdb;

int elektraXercesOpen (Plugin * handle, Key * errorKey)
{
	XERCES_CPP_NAMESPACE::XMLPlatformUtils::Initialize ();
	return 1; // success
}

int elektraXercesClose (Plugin * handle, Key * errorKey)
{
	XERCES_CPP_NAMESPACE::XMLPlatformUtils::Terminate ();
	return 1; // success
}

int elektraXercesGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/xerces"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/xerces", KEY_VALUE, "xerces plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/xerces/exports", KEY_END),
			       keyNew ("system/elektra/modules/xerces/exports/open", KEY_FUNC, elektraXercesOpen, KEY_END),
			       keyNew ("system/elektra/modules/xerces/exports/close", KEY_FUNC, elektraXercesClose, KEY_END),
			       keyNew ("system/elektra/modules/xerces/exports/get", KEY_FUNC, elektraXercesGet, KEY_END),
			       keyNew ("system/elektra/modules/xerces/exports/set", KEY_FUNC, elektraXercesSet, KEY_END),
			       keyNew ("system/elektra/modules/xerces/exports/error", KEY_FUNC, elektraXercesError, KEY_END),
			       keyNew ("system/elektra/modules/xerces/exports/checkconf", KEY_FUNC, elektraXercesCheckConfig, KEY_END),
#include ELEKTRA_README (xerces)
			       keyNew ("system/elektra/modules/xerces/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	// TODO error handling
	kdb::KeySet ks (returned);
	deserialize (keyString (parentKey), ks);
	// Avoid destruction of the ks at the end
	ks.release ();
	return 1; // success
}

int elektraXercesSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	// set all keys
	// this function is optional

	return 1; // success
}

int elektraXercesError (Plugin * handle, KeySet * returned, Key * parentKey)
{
	// handle errors (commit failed)
	// this function is optional

	return 1; // success
}

int elektraXercesCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (xerces)
{
	// clang-format off
	return elektraPluginExport("xerces",
		ELEKTRA_PLUGIN_OPEN,  &elektraXercesOpen,
		ELEKTRA_PLUGIN_CLOSE, &elektraXercesClose,
		ELEKTRA_PLUGIN_GET,   &elektraXercesGet,
		ELEKTRA_PLUGIN_SET,   &elektraXercesSet,
		ELEKTRA_PLUGIN_ERROR, &elektraXercesError,
		ELEKTRA_PLUGIN_END);
}

