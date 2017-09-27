/**
 * @file
 *
 * @brief Read key sets using yaml-cpp
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "read.hpp"
#include "yaml-cpp/yaml.h"

#include <kdb.hpp>
#include <kdbease.h>
#include <kdblogger.h>
#include <kdbplugin.h>

#include <sstream>

using namespace std;
using namespace kdb;

namespace
{
/**
 * @brief This function creates a new key from the given parameters.
 *
 * @param name This string specifies the postfix of the name of the key produced by this function.
 * @param parent This key specifies the prefix of the name of the key produced by this function.
 *
 * @returns The function returns a new key that combines the name of the parent key and `name`.
 */
Key newKey (string const & name, Key const & parent)
{
	Key key{ parent.getFullName (), KEY_END };
	key.addBaseName (name);

	return key;
}

/**
 * @brief This function creates a new array key from the given parameters.
 *
 * @param mappings This argument specifies the key set of the new key this function creates.
 * @param arrayKey This argument specifies the key that represents the root of the array.
 *
 * @returns The function returns a new key that is part of the array represented by `arrayKey`.
 */
Key newArrayKey (KeySet const & mappings, Key & arrayKey)
{
	KeySet arrayEntries{ elektraArrayGet (arrayKey.getKey (), mappings.getKeySet ()) };

	if (arrayEntries.size () <= 0)
	{
		Key first = arrayKey.dup ();
		first.addBaseName ("#");
		arrayEntries.append (first);
	}

	arrayKey.setMeta ("array", arrayEntries.size ());

	return elektraArrayGetNextKey (arrayEntries.getKeySet ());
}

/**
 * @brief Convert a YAML node to a key set
 *
 * @param node This YAML node stores the data that should be added to the keyset `mappings`
 * @param mappings The key set where the YAML data will be stored
 * @param prefix This key stores the prefix for the key name
 */
void convertNodeToKeySet (YAML::Node const & node, KeySet & mappings, Key & parent)
{
	if (node.IsScalar ())
	{
		Key key (parent.getFullName (), KEY_VALUE, node.as<string> ().c_str (), KEY_END);
		ELEKTRA_LOG_DEBUG ("%s: %s", key.getName ().c_str (), key.get<string> ().c_str ());
		mappings.append (key);
	}
	else if (node.IsMap () || node.IsSequence ())
	{
		for (auto element : node)
		{
			Key key = node.IsMap () ? newKey (element.first.as<string> (), parent) : newArrayKey (mappings, parent);
			mappings.append (key);
			convertNodeToKeySet (node.IsMap () ? element.second : element, mappings, key);
		}
	}
}
} // end namespace

/**
 * @brief Read a YAML file and add the resulting data to a given key set
 *
 * @param mappings The key set where the YAML data will be stored
 * @param parent This key stores the path to the YAML data file that should be read
 */
void yamlcpp::yamlRead (KeySet & mappings, Key & parent)
{
	YAML::Node config = YAML::LoadFile (parent.getString ());
	ostringstream data;
	data << config;

	ELEKTRA_LOG_DEBUG ("Data: “%s”", data.str ().c_str ());
	convertNodeToKeySet (config, mappings, parent);
	ELEKTRA_LOG_DEBUG ("Number of keys: %zd", mappings.size ());
}
