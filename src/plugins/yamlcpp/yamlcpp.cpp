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
#include "yaml.h"

#include <kdb.hpp>
#include <kdbease.h>
#include <kdblogger.h>

#include <fstream>
#include <sstream>
#include <string>

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

/**
 * @brief Read a YAML file and add the resulting data to a given key set
 *
 * @param mappings The key set where the YAML data will be stored
 * @param parent This key stores the path to the YAML data file that should be read
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if reading, parsing and storing was successful
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if there were any problems
 */
static int yamlRead (kdb::KeySet & mappings, kdb::Key & parent)
{
	using namespace kdb;
	using namespace std;

	YAML::Node config = YAML::LoadFile (parent.getString ());
	ostringstream data;
	data << config;

	ELEKTRA_LOG_DEBUG ("Data: “%s”", data.str ().c_str ());
	for (auto element : config)
	{
		Key key (parent.getFullName (), KEY_END);
		key.addBaseName (element.first.as<string> ());
		key.set<string> (element.second.as<string> ());
		ELEKTRA_LOG_DEBUG ("%s: %s", key.get<string> ().c_str (), key.getName ().c_str ());
		mappings.append (key);
	}
	ELEKTRA_LOG_DEBUG ("Number of keys: %zd", mappings.size ());

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * @brief This function saves the key-value pairs stored in `mappings` as YAML data in the location specified via `parent`.
 *
 * @param mappings This key set stores the mappings that should be saved as YAML data.
 * @param parent This key specifies the path to the YAML data file that should be written.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if writing was successful
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if there were any problems
 */
static int yamlWrite (kdb::KeySet & mappings, kdb::Key & parent)
{
	using namespace kdb;
	using namespace std;

	ofstream output (parent.getString ());
	YAML::Emitter emitter (output);
	emitter << YAML::BeginMap;

	for (auto key : mappings)
	{
		const char * name = elektraKeyGetRelativeName (key.getKey (), parent.getKey ());
		emitter << YAML::Key << name << YAML::Value << key.get<string> ();
		ELEKTRA_LOG_DEBUG ("%s: %s", key.get<string> ().c_str (), key.getName ().c_str ());
	}

	emitter << YAML::EndMap;

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

extern "C" {
using namespace ckdb;

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
		      keyNew ("system/elektra/modules/yamlcpp/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
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

	int status = yamlRead (keys, parent);

	parent.release ();
	keys.release ();

	return status;
}

/** @see elektraDocSet */
int elektraYamlcppSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	kdb::Key parent = kdb::Key (parentKey);
	kdb::KeySet keys = kdb::KeySet (returned);

	int status = yamlWrite (keys, parent);

	parent.release ();
	keys.release ();

	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yamlcpp)
{
	return elektraPluginExport ("yamlcpp", ELEKTRA_PLUGIN_GET, &elektraYamlcppGet, ELEKTRA_PLUGIN_SET, &elektraYamlcppSet,
				    ELEKTRA_PLUGIN_END);
}

} // end extern "C"
