/**
 * @file
 *
 * @brief Write key sets using yaml-cpp
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "write.hpp"
#include "yaml-cpp/yaml.h"

#include <kdbease.h>
#include <kdblogger.h>
#include <kdbplugin.h>

#include <fstream>

using namespace std;
using namespace kdb;

namespace
{

/**
 * @brief This function returns a `NameIterator` starting at the first level that is not part of `parent`.
 *
 * @pre The parameter `key` must be a child of `parent`.
 *
 * @param key This is the key for which this function returns a relative iterator.
 * @param parent This key specifies the part of the name iterator that will not be part of the return value of this function.
 *
 * @returns A relative iterator that starts with the first part of the name of `key` not contained in `parent`.
 */
NameIterator relativeKeyIterator (Key const & key, Key const & parent)
{
	auto parentIterator = parent.begin ();
	auto keyIterator = key.begin ();
	while (parentIterator != parent.end () && keyIterator != key.end ())
	{
		parentIterator++;
		keyIterator++;
	}
	return keyIterator;
}

/**
 * @brief This function checks if a key name specifies an array key.
 *
 * If the key name contains a valid array index that is smaller than `unsigned long long`, then the function will also return this index.
 *
 * @param nameIterator This iterator specifies the name of the key.
 *
 * @retval (true, arrayIndex) if `name` specifies an array key, where `arrayIndex` specifies the index stored in the array key.
 * @retval (false, 0) otherwise
 */
std::pair<bool, unsigned long long> isArrayIndex (NameIterator const & nameIterator)
{
	string const name = *nameIterator;
	auto const offsetIndex = ckdb::elektraArrayValidateBaseNameString (name.c_str ());
	auto const isArrayElement = offsetIndex >= 1;
	return { isArrayElement, isArrayElement ? stoull (name.substr (offsetIndex)) : 0 };
}

/**
 * @brief This function creates a YAML node representing a key value.
 *
 * @param key This key specifies the data that should be saved in the YAML node returned by this function.
 *
 * @note Since YAML does not support non-empty binary data directly this function replaces data stored in binary keys with the string
 *       `Unsupported binary value!`. If you need support for binary data, please load the Base64 before you use YAML CPP.
 *
 * @returns A new YAML node containing the data specified in `key`
 */
YAML::Node createMetaDataNode (Key const & key)
{
	return key.hasMeta ("array") ?
		       YAML::Node (YAML::NodeType::Sequence) :
		       key.getBinarySize () == 0 ? YAML::Node (YAML::NodeType::Null) :
						   YAML::Node (key.isBinary () ? "Unsupported binary value!" : key.getString ());
}

/**
 * @brief This function creates a YAML Node containing a key value and optionally metadata.
 *
 * @param key This key specifies the data that should be saved in the YAML node returned by this function.
 *
 * @note Since YAML does not support non-empty binary data directly this function replaces data stored in binary keys with the string
 *       `Unsupported binary value!`. If you need support for binary data, please load the Base64 before you use YAML CPP.
 *
 * @returns A new YAML node containing the data and metadata specified in `key`
 */
YAML::Node createLeafNode (Key & key)
{

	YAML::Node metaNode{ YAML::Node (YAML::NodeType::Map) };
	YAML::Node dataNode = createMetaDataNode (key);

	key.rewindMeta ();
	while (Key meta = key.nextMeta ())
	{
		if (meta.getName () == "array" || meta.getName () == "binary") continue;
		if (meta.getName () == "type" && meta.getString () == "binary")
		{
			dataNode.SetTag ("tag:yaml.org,2002:binary");
			continue;
		}
		metaNode[meta.getName ()] = meta.getString ();
		ELEKTRA_LOG_DEBUG ("Add metakey “%s: %s”", meta.getName ().c_str (), meta.getString ().c_str ());
	}

	if (metaNode.size () <= 0)
	{
		ELEKTRA_LOG_DEBUG ("Return leaf node with value “%s”",
				   dataNode.IsNull () ? "~" : dataNode.IsSequence () ? "[]" : dataNode.as<string> ().c_str ());
		return dataNode;
	}

	YAML::Node node{ YAML::Node (YAML::NodeType::Sequence) };
	node.SetTag ("!elektra/meta");
	node.push_back (dataNode);
	node.push_back (metaNode);

#ifdef HAVE_LOGGER
	ostringstream data;
	data << node;
	ELEKTRA_LOG_DEBUG ("Return meta leaf node with value “%s”", data.str ().c_str ());
#endif

	return node;
}

/**
 * @brief This function adds `null` elements to the given YAML collection.
 *
 * @param sequence This node stores the collection to which this function adds `numberOfElements` empty elements.
 * @param numberOfElements This parameter specifies the number of empty element this function adds to `sequence`.
 */
void addEmptyArrayElements (YAML::Node & sequence, unsigned long long const numberOfElements)
{
	ELEKTRA_LOG_DEBUG ("Add %lld empty array elements", numberOfElements);
	for (auto missingFields = numberOfElements; missingFields > 0; missingFields--)
	{
		sequence.push_back ({});
	}
}

/**
 * @brief This function adds a key to a YAML node.
 *
 * @param data This node stores the data specified via `keyIterator`.
 * @param keyIterator This iterator specifies the current part of the key name this function adds to `data`.
 * @param key This parameter specifies the key that should be added to `data`.
 */
void addKey (YAML::Node & data, NameIterator & keyIterator, Key & key)
{
	auto const isArrayAndIndex = isArrayIndex (keyIterator);
	auto const isArray = isArrayAndIndex.first;
	auto const arrayIndex = isArrayAndIndex.second;

	if (data.IsScalar ()) data = YAML::Node (YAML::NodeType::Undefined);

#ifdef HAVE_LOGGER
	ostringstream output;
	output << data;
	ELEKTRA_LOG_DEBUG ("Add key part “%s” to node “%s”", (*keyIterator).c_str (), output.str ().c_str ());
#endif

	if (keyIterator == key.end ())
	{
		ELEKTRA_LOG_DEBUG ("Create leaf node for key “%s”", key.getName ().c_str ());
		data = createLeafNode (key);
		return;
	}
	if (keyIterator == --key.end ())
	{
		if (isArray)
		{
			addEmptyArrayElements (data, arrayIndex - data.size ());
			data.push_back (createLeafNode (key));
		}
		else
		{
			data[*keyIterator] = createLeafNode (key);
		}

		return;
	}

	YAML::Node node;

	if (isArray)
	{
		node = (data[arrayIndex] && !data[arrayIndex].IsScalar ()) ? data[arrayIndex] : YAML::Node ();
		data[arrayIndex] = node;
	}
	else
	{
		node = (data[*keyIterator] && !data[*keyIterator].IsScalar ()) ? data[*keyIterator] : YAML::Node ();
		data[*keyIterator] = node;
	}
	addKey (node, ++keyIterator, key);
}

/**
 * @brief This function adds a key set to a YAML node.
 *
 * @param data This node stores the data specified via `mappings`.
 * @param mappings This keyset specifies all keys and values this function adds to `data`.
 * @param parent This key is the root of all keys stored in `mappings`.
 */
void addKeys (YAML::Node & data, KeySet const & mappings, Key const & parent)
{
	for (auto key : mappings)
	{
		ELEKTRA_LOG_DEBUG ("Convert key “%s”: “%s”", key.getName ().c_str (),
				   key.getBinarySize () == 0 ? "NULL" : key.isString () ? key.getString ().c_str () : "binary value!");
		NameIterator keyIterator = relativeKeyIterator (key, parent);
		addKey (data, keyIterator, key);

#ifdef HAVE_LOGGER
		ostringstream output;
		output << data;
		ELEKTRA_LOG_DEBUG ("Converted key data “%s”", output.str ().c_str ());
#endif
	}
}

} // end namespace

/**
 * @brief This function saves the key-value pairs stored in `mappings` as YAML data in the location specified via `parent`.
 *
 * @param mappings This key set stores the mappings that should be saved as YAML data.
 * @param parent This key specifies the path to the YAML data file that should be written.
 */
void yamlcpp::yamlWrite (KeySet const & mappings, Key const & parent)
{
	ofstream output (parent.getString ());
	auto data = YAML::Node ();
	addKeys (data, mappings, parent);

#ifdef HAVE_LOGGER
	ostringstream outputString;
	outputString << data;

	ELEKTRA_LOG_DEBUG ("Write data “%s”", outputString.str ().c_str ());
#endif

	output << data;
}
