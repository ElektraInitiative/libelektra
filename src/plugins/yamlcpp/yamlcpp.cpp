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
#include "read.hpp"
#include "write.hpp"
using namespace yamlcpp;

#include <kdb.hpp>
using namespace ckdb;
#include <kdberrors.h>
#include <kdblogger.h>

#include "yaml-cpp/yaml.h"

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

/**
 * @brief This function returns a key set containing the contract of this plugin.
 *
 * @return A contract describing the functionality of this plugin.
 */
static KeySet * contractYamlCpp (void)
{
	return ksNew (30, keyNew ("system/elektra/modules/yamlcpp", KEY_VALUE, "yamlcpp plugin waits for your orders", KEY_END),
		      keyNew ("system/elektra/modules/yamlcpp/exports", KEY_END),
		      keyNew ("system/elektra/modules/yamlcpp/exports/get", KEY_FUNC, elektraYamlcppGet, KEY_END),
		      keyNew ("system/elektra/modules/yamlcpp/exports/set", KEY_FUNC, elektraYamlcppSet, KEY_END),
#include ELEKTRA_README (yamlcpp)
		      keyNew ("system/elektra/modules/yamlcpp/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
		      keyNew ("system/elektra/modules/yamlcpp/config/needs/binary/meta", KEY_VALUE, "true", KEY_END), KS_END);
}

// ====================
// = Plugin Interface =
// ====================

/** @see elektraDocGet */
int elektraYamlcppGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (std::string (keyName (parentKey)) == "system/elektra/modules/yamlcpp")
	{
		KeySet * contract = contractYamlCpp ();
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	kdb::Key parent = kdb::Key (parentKey);
	kdb::KeySet keys = kdb::KeySet (returned);

	ELEKTRA_LOG_DEBUG ("Read file “%s”", parent.getString ().c_str ());

	int status = ELEKTRA_PLUGIN_STATUS_ERROR;

	try
	{
		yamlRead (keys, parent);
		status = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	catch (YAML::ParserException const & exception)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_PARSE, parent.getKey (), "Unable to parse file “%s”: %s.", parent.getString ().c_str (),
				    exception.what ());
	}
	catch (YAML::RepresentationException const & exception)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_YAMLCPP_REPRESENTATION, parent.getKey (), "Unable to read data from file “%s”: %s",
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

	int status = ELEKTRA_PLUGIN_STATUS_ERROR;

	try
	{
		yamlWrite (keys, parent);
		status = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	catch (YAML::BadFile const & exception)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_YAMLCPP_WRITE_FAILED, parent.getKey (), "Unable to write to file “%s”: %s.",
				    parent.getString ().c_str (), exception.what ());
	}
	catch (YAML::EmitterException const & exception)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_YAMLCPP_EMITTER_FAILED, parent.getKey (),
				    "Something went wrong while emitting YAML data to file “%s”: %s.", parent.getString ().c_str (),
				    exception.what ());
	}

	parent.release ();
	keys.release ();

	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yamlcpp)
{
	return elektraPluginExport ("yamlcpp", ELEKTRA_PLUGIN_GET, &elektraYamlcppGet, ELEKTRA_PLUGIN_SET, &elektraYamlcppSet,
				    ELEKTRA_PLUGIN_END);
}
