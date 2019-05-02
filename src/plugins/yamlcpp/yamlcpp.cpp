/**
 * @file
 *
 * @brief Source for yamlcpp plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include "yamlcpp.hpp"
#include "log.hpp"
#include "read.hpp"
#include "write.hpp"
using namespace yamlcpp;

#include <kdb.hpp>
using namespace ckdb;
#include <kdberrors.h>
#include <kdblogger.h>

#include "yaml-cpp/yaml.h"

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

namespace
{

/**
 * @brief This function returns a key set containing the contract of this plugin.
 *
 * @return A contract describing the functionality of this plugin.
 */
kdb::KeySet contractYamlCpp (void)
{
	return kdb::KeySet{ 30,
			    keyNew ("system/elektra/modules/yamlcpp", KEY_VALUE, "yamlcpp plugin waits for your orders", KEY_END),
			    keyNew ("system/elektra/modules/yamlcpp/exports", KEY_END),
			    keyNew ("system/elektra/modules/yamlcpp/exports/get", KEY_FUNC, elektraYamlcppGet, KEY_END),
			    keyNew ("system/elektra/modules/yamlcpp/exports/set", KEY_FUNC, elektraYamlcppSet, KEY_END),
#include ELEKTRA_README
			    keyNew ("system/elektra/modules/yamlcpp/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			    keyNew ("system/elektra/modules/yamlcpp/config/needs/binary/meta", KEY_VALUE, "true", KEY_END),
			    keyNew ("system/elektra/modules/yamlcpp/config/needs/boolean/restore", KEY_VALUE, "#1", KEY_END),
			    KS_END };
}
}

// ====================
// = Plugin Interface =
// ====================

/** @see elektraDocGet */
int elektraYamlcppGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	kdb::Key parent = kdb::Key (parentKey);
	kdb::KeySet keys = kdb::KeySet (returned);

	if (parent.getName () == "system/elektra/modules/yamlcpp")
	{
		keys.append (contractYamlCpp ());
		parent.release ();
		keys.release ();

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	int status = ELEKTRA_PLUGIN_STATUS_ERROR;

	try
	{
		yamlRead (keys, parent);
		status = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	catch (YAML::ParserException const & exception)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parent.getKey (), "Unable to parse file “%s”: %s.", parent.getString ().c_str (),
							 exception.what ());
	}
	catch (std::overflow_error const & exception)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INSERT_VALUE_ARRAY, parent.getKey (), "Unable to read data from file “%s”: %s.",
				    parent.getString ().c_str (), exception.what ());
	}
	catch (YAML::RepresentationException const & exception)
	{
		ELEKTRA_SET_GENERAL_RESOURCE_ERRORF (parent.getKey (), "Unable to read data from file “%s”: %s",
						     parent.getString ().c_str (), exception.what ());
	}

	parent.release ();
	keys.release ();

	return status;
}

/** @see elektraDocSet */
int elektraYamlcppSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	kdb::Key parent = kdb::Key (parentKey);
	kdb::KeySet keys = kdb::KeySet (returned);

#ifdef HAVE_LOGGER
	ELEKTRA_LOG_DEBUG ("Write keys:");
	logKeySet (keys);
#endif

	int status = ELEKTRA_PLUGIN_STATUS_ERROR;

	try
	{
		yamlWrite (keys, parent);
		status = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	catch (YAML::BadFile const & exception)
	{
		ELEKTRA_SET_GENERAL_RESOURCE_ERRORF (parent.getKey (), "Unable to write to file “%s”: %s.", parent.getString ().c_str (),
						     exception.what ());
	}
	catch (YAML::EmitterException const & exception)
	{
		ELEKTRA_SET_ASSERTION_ERRORF (parent.getKey (), "Something went wrong while emitting YAML data to file “%s”: %s.",
					      parent.getString ().c_str (), exception.what ());
	}

	parent.release ();
	keys.release ();

	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("yamlcpp", ELEKTRA_PLUGIN_GET, &elektraYamlcppGet, ELEKTRA_PLUGIN_SET, &elektraYamlcppSet,
				    ELEKTRA_PLUGIN_END);
}
