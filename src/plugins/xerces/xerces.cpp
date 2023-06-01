/**
 * @file this file contains the entry point to the plugin as a gateway between c and c++
 *
 * @brief Plugin enables storage to xml files via the Xerces library
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./xerces.hpp"
#include "./deserializer.hpp"
#include "./serializer.hpp"
#include "./util.hpp"

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>
#include <xercesc/util/PlatformUtils.hpp>


#include <elektra/core/errors.h>
#include <internal/macros/attributes.h>
#include <internal/utility/old_helper.h>

using namespace ckdb;
using namespace xerces;

XERCES_CPP_NAMESPACE_USE

int elektraXercesOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	XMLPlatformUtils::Initialize ();
	return 1;
}

int elektraXercesClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	XMLPlatformUtils::Terminate ();
	return 1;
}

int elektraXercesGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/xerces"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/xerces", KEY_VALUE, "xerces plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/xerces/exports", KEY_END),
			       keyNew ("system:/elektra/modules/xerces/exports/open", KEY_FUNC, elektraXercesOpen, KEY_END),
			       keyNew ("system:/elektra/modules/xerces/exports/close", KEY_FUNC, elektraXercesClose, KEY_END),
			       keyNew ("system:/elektra/modules/xerces/exports/get", KEY_FUNC, elektraXercesGet, KEY_END),
			       keyNew ("system:/elektra/modules/xerces/exports/set", KEY_FUNC, elektraXercesSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/xerces/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1;
	}

	kdb::KeySet ks (returned);
	kdb::Key k (parentKey);
	int ret = 0;
	// Bridge the C++ exceptions to elektra error messages
	try
	{
		deserialize (k, ks);
		ret = 1;
	}
	catch (const OutOfMemoryException & e)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
	}
	catch (const XMLException & e)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, asCStr (e.getMessage ()));
	}
	catch (const DOMException & e)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, asCStr (e.getMessage ()));
	}
	catch (const XercesPluginException & e)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, e.what ());
	}
	catch (...)
	{
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERROR (parentKey, "Unknown exception occurred while reading xml file");
	}

	// Avoid destruction of the pointers at the end
	k.release ();
	ks.release ();
	return ret;
}

int elektraXercesSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	// Bridge the C++ exceptions to elektra error messages
	kdb::KeySet ks (returned);
	kdb::Key k (parentKey);
	int ret = 0;
	try
	{
		serialize (k, ks);
		ret = 1;
	}
	catch (const OutOfMemoryException & e)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
	}
	catch (const XMLException & e)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, asCStr (e.getMessage ()));
	}
	catch (const DOMException & e)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, asCStr (e.getMessage ()));
	}
	catch (const XercesPluginException & e)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, e.what ());
	}
	catch (...)
	{
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERROR (parentKey, "Unknown exception occurred while writing xml file");
	}

	// Avoid destruction of the pointers at the end
	k.release ();
	ks.release ();
	return ret;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("xerces",
		ELEKTRA_PLUGIN_OPEN,  &elektraXercesOpen,
		ELEKTRA_PLUGIN_CLOSE, &elektraXercesClose,
		ELEKTRA_PLUGIN_GET,   &elektraXercesGet,
		ELEKTRA_PLUGIN_SET,   &elektraXercesSet,
		ELEKTRA_PLUGIN_END);
}
