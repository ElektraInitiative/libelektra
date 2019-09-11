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
#include <kdblogger.h>
#include <kdbplugin.h>

#include <sstream>

using namespace std;
using namespace kdb;

namespace
{
/**
 * @brief This function converts a given number to an array base name.
 *
 * @param index This number specifies the index of the array entry.
 *
 * @return A string representing the given indices as Elektra array name.
 */
string indexToArrayBaseName (uintmax_t const index)
{
	size_t digits = 1;

	for (uintmax_t value = index; value > 9; digits++)
	{
		value /= 10;
	}

	return "#" + string (digits - 1, '_') + to_string (index);
}

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
	ELEKTRA_LOG_DEBUG ("Add new key with base name “%s”", name.c_str ());

	Key key{ parent.getFullName (), KEY_BINARY, KEY_END };
	key.addBaseName (name);

	return key;
}

/**
 * @brief This function creates a new array key from the given parameters.
 *
 * @param arrayKey This argument specifies the key that represents the root of the array.
 * @param index This parameter specifies the index of the array key this function creates.
 *
 * @returns The function returns a new key that is part of the array represented by `arrayKey`.
 */
Key newArrayKey (Key & arrayKey, uintmax_t const index)
{
	ELEKTRA_LOG_DEBUG ("Add new array element to array parent “%s”", arrayKey.getName ().c_str ());

	Key newKey{ arrayKey.getName (), KEY_BINARY, KEY_END };
	newKey.addBaseName (indexToArrayBaseName (index));
	arrayKey.setMeta ("array", newKey.getBaseName ());

	return newKey;
}

/**
 * @brief Add metadata saved in a YAML map to the specified key
 *
 * @param key This parameter saves the key to which this function should add the metadata stored in `node`.
 * @param node This YAML node stores a map containing metadata.
 */
void addMetadata (Key & key, YAML::Node const & node)
{
	for (auto & element : node)
	{
		auto metakey = element.first.as<string> ();
		auto metavalue = element.second.IsNull () ? "" : element.second.as<string> ();
		ELEKTRA_LOG_DEBUG ("Add metakey “%s: %s”", metakey.c_str (), metavalue.c_str ());
		key.setMeta (metakey, metavalue);
	}
}

/**
 * @brief Create a key containing a (possibly empty) value.
 *
 * @param node This YAML node stores the data that should be converted to a new `Key`.
 * @param name This text specifies the name of the key this function creates.
 *
 * @return A new key containing the data specified in `node`
 */
Key createLeafKey (YAML::Node const & node, string const & name)
{
	Key key{ name, KEY_BINARY, KEY_END };
	if (!node.IsNull ())
	{
		auto value = node.as<string> ();
		if (value == "true" || value == "false")
		{
			try
			{
				key.set<bool> (node.as<bool> ());
			}
			catch (YAML::BadConversion const &)
			{
				key.set<string> (value); // Save value as string, if `node` is a quoted scalar
			}
		}
		else
		{
			key.set<string> (value);
		}
	}
	if (node.Tag () == "tag:yaml.org,2002:binary")
	{
		ELEKTRA_LOG_DEBUG ("Set metadata type of key to binary");
		key.setMeta ("type", "binary");
	}
	ELEKTRA_LOG_DEBUG ("Add key “%s: %s”", key.getName ().c_str (),
			   key.getBinarySize () == 0 ? "NULL" : key.isBinary () ? "binary value!" : key.get<string> ().c_str ());
	return key;
}

/**
 * @brief Convert the key value of a YAML meta node to a key
 *
 * @param node This YAML meta node stores the data this function stores in the returned key
 * @param parent This key stores the prefix for the key name
 *
 * @return A key representing the key value stored in `node`
 */
Key convertMetaNodeToKey (YAML::Node const & node, Key & parent)
{
	auto key = node[0].IsNull () ? Key{ parent.getFullName (), KEY_BINARY, KEY_END } :
				       Key{ parent.getFullName (), KEY_VALUE, node[0].as<string> ().c_str (), KEY_END };
	ELEKTRA_LOG_DEBUG ("Add key “%s”: “%s”", key.getName ().c_str (),
			   key.getBinarySize () == 0 ? "NULL" : key.isString () ? key.getString ().c_str () : "binary value!");
	return key;
}

/**
 * @brief Convert a YAML node to a key set
 *
 * @param node This YAML node stores the data that should be added to the keyset `mappings`
 * @param mappings The key set where the YAML data will be stored
 * @param parent This key stores the prefix for the key name
 */
void convertNodeToKeySet (YAML::Node const & node, KeySet & mappings, Key & parent)
{
	if (node.Tag () == "!elektra/meta")
	{
		auto key = convertMetaNodeToKey (node, parent);
		mappings.append (key);
		addMetadata (key, node[1]);
	}
	else if (node.IsScalar () || node.IsNull ())
	{
		auto key = createLeafKey (node, parent.getFullName ());
		mappings.append (key);
	}
	else if (node.IsMap ())
	{
		for (auto element : node)
		{
			Key key = newKey (element.first.as<string> (), parent);
			convertNodeToKeySet (element.second, mappings, key);
		}
	}
	else if (node.IsSequence ())
	{
		uintmax_t index = 0;
		uintmax_t lastIndex = 0;
		for (auto element : node)
		{
			if (lastIndex == UINTMAX_MAX)
			{
				Key key = newArrayKey (parent, lastIndex);
				throw std::overflow_error ("Unable to add element after “" + key.getName () + "”" + "in array “" +
							   parent.getName () + "”");
			}
			Key key = newArrayKey (parent, index);
			mappings.append (parent); // Update array metadata
			convertNodeToKeySet (element, mappings, key);
			lastIndex = index++;
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

	ELEKTRA_LOG_DEBUG ("Read file “%s”", parent.getString ().c_str ());

#ifdef HAVE_LOGGER
	ostringstream data;
	data << config;

	ELEKTRA_LOG_DEBUG ("Read Data:");
	ELEKTRA_LOG_DEBUG ("——————————");

	istringstream stream (data.str ());
	for (string line; std::getline (stream, line);)
	{
		ELEKTRA_LOG_DEBUG ("%s", line.c_str ());
	}

	ELEKTRA_LOG_DEBUG ("——————————");
#endif

	convertNodeToKeySet (config, mappings, parent);
	ELEKTRA_LOG_DEBUG ("Added %zd key%s", mappings.size (), mappings.size () == 1 ? "" : "s");
}
