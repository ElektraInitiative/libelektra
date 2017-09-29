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
	if (name.size () < 2 || name.front () != '#') return std::make_pair (false, 0);

	auto errnoValue = errno;

	try
	{
		return std::make_pair (true, stoull (name.substr (name.find_first_not_of ("#\\_"))));
	}
	catch (invalid_argument)
	{
		return std::make_pair (false, 0);
	}
	catch (out_of_range)
	{
		errno = errnoValue;
		return std::make_pair (false, 0);
	}
}

/**
 * @brief This function creates a YAML Node containing a key value and optionally metadata.
 *
 * @param key This key specifies the data that should be saved in the YAML node returned by this function.
 *
 * @returns A new YAML node containing the data and metadata specified in `key`
 */
YAML::Node createLeafNode (Key & key)
{
	key.rewindMeta ();

	auto metaNode{ YAML::Node (YAML::NodeType::Map) };
	Key meta;
	while ((meta = key.nextMeta ()))
	{
		if (meta.getName () == "array") continue;
		metaNode[meta.getName ()] = meta.getString ();
		ELEKTRA_LOG_DEBUG ("Add metakey “%s: %s”", meta.getName ().c_str (), meta.getString ().c_str ());
	}

	if (metaNode.size () <= 0)
	{
		ELEKTRA_LOG_DEBUG ("Return leaf node with value “%s”", key.getString ().c_str ());
		return YAML::Node (key.getString ());
	}

	auto node{ YAML::Node (YAML::NodeType::Sequence) };
	node.SetTag ("!elektra/meta");
	node.push_back (key.getString ());
	node.push_back (metaNode);

#ifdef LOGGING_ENABLED
	ostringstream data;
	data << node;
	ELEKTRA_LOG_DEBUG ("Return meta leaf node with value “%s”", data.str ().c_str ());
#endif

	return node;
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

	if (keyIterator == --key.end ())
	{
		if (isArray)
		{
			data[arrayIndex] = createLeafNode (key);
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
		ELEKTRA_LOG_DEBUG ("Convert key “%s: %s”", key.getName ().c_str (), key.get<string> ().c_str ());
		NameIterator keyIterator = relativeKeyIterator (key, parent);
		addKey (data, keyIterator, key);

#ifdef LOGGING_ENABLED
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

#ifdef LOGGING_ENABLED
	ostringstream outputString;
	outputString << data;

	ELEKTRA_LOG_DEBUG ("Write data “%s”", outputString.str ().c_str ());
#endif

	output << data;
}
