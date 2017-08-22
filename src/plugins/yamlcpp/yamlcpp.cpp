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

static int yamlWrite (kdb::KeySet & mappings, kdb::Key & parent)
{
	using namespace kdb;
	using namespace std;

	YAML::Node config;

	for (auto key : mappings)
	{
		const char * name = elektraKeyGetRelativeName (key.getKey (), parent.getKey ());
		config[name] = key.get<string> ();
		ELEKTRA_LOG_DEBUG ("%s: %s", key.get<string> ().c_str (), key.getName ().c_str ());
	}

	ofstream output (parent.getString ());
	output << config;

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
