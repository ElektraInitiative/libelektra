/**
 * @file
 *
 * @brief Write key sets using yaml-cpp
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "write.hpp"
#include "log.hpp"
#include "yaml-cpp/yaml.h"

#include <kdbassert.h>
#include <kdbease.h>
#include <kdblogger.h>
#include <kdbplugin.h>

#include <fstream>
#include <stack>

using namespace std;
using namespace kdb;

namespace
{

using KeySetPair = pair<KeySet, KeySet>;

/**
 * @brief This function returns all array parents for a given key set.
 *
 * @param keys This parameter contains the key set this function searches for array parents.
 *
 * @return A key set that contains all array parents stored in `keys`
 */
KeySet splitArrayParents (KeySet const & keys)
{
	KeySet arrayParents;
	for (auto const & key : keys)
	{
		if (key.hasMeta ("array")) arrayParents.append (key);
	}

#ifdef HAVE_LOGGER
	ELEKTRA_LOG_DEBUG ("Array parents:");
	logKeySet (arrayParents);
#endif

	return arrayParents;
}

/**
 * @brief This function splits `keys` into two key sets, one for array parents and elements, and the other one for all other keys.
 *
 * @param arrayParents This key set contains (a copy of) all array parents of `keys`.
 * @param keys This parameter contains the key set this function splits.
 *
 * @return A pair of key sets, where the first key set contains all array parents and elements,
 *         and the second key set contains all other keys
 */
KeySetPair splitArrayOther (KeySet const & arrayParents, KeySet const & keys)
{
	KeySet others = keys.dup ();
	KeySet arrays;

	for (auto const & parent : arrayParents)
	{
		arrays.append (others.cut (parent));
	}

	return make_pair (arrays, others);
}

/**
 * @brief This function determines all keys “missing” from the given keyset.
 *
 * The term “missing” refers to keys that are not part of the hierarchy. For example in a key set with the parent key
 *
 *  - `user/parent`
 *
 * that contains the keys
 *
 * - `user/parent/level1/level2`, and
 * - `user/parent/level1/level2/level3/level4`
 *
 * , the keys
 *
 * - `user/parent/level1`, and
 * - `user/parent/level1/level2/level3`
 *
 * are missing.
 *
 * @param keys This parameter contains the key set for which this function determines missing keys.
 * @param parent This value stores the parent key of `keys`.
 *
 * @return A key set that contains all keys missing from `keys`
 */
KeySet missingKeys (KeySet const & keys, Key const & parent)
{
	KeySet missing;

	keys.rewind ();
	Key previous{ parent.getName (), KEY_BINARY, KEY_END };
	for (; keys.next (); previous = keys.current ())
	{
		if (keys.current ().isDirectBelow (previous) || !keys.current ().isBelow (previous)) continue;

		Key current{ keys.current ().getName (), KEY_BINARY, KEY_END };
		while (!current.isDirectBelow (previous))
		{
			ckdb::keySetBaseName (*current, NULL);
			missing.append (current);
			current = current.dup ();
		}
	}

#ifdef HAVE_LOGGER
	ELEKTRA_LOG_DEBUG ("Add missing keys:");
	logKeySet (missing);
#endif

	return missing;
}

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
 * @brief This function returns the array index for a given key part.
 *
 * @param nameIterator This iterator specifies the name of the key.
 *
 * @retval The index of the array element, or `0` if the given key part is not an array element.
 */
unsigned long long getArrayIndex (NameIterator const & nameIterator)
{
	string const name = *nameIterator;
	auto const offsetIndex = ckdb::elektraArrayValidateBaseNameString (name.c_str ());
	auto const isArrayElement = offsetIndex >= 1;
	return isArrayElement ? stoull (name.substr (static_cast<size_t> (offsetIndex))) : 0;
}

/**
 * @brief This function creates a YAML node representing a key value.
 *
 * @param key This key specifies the data that should be saved in the YAML node returned by this function.
 *
 * @note Since YAML does not support non-empty binary data directly this function replaces data stored in binary keys with the string
 *       `Unsupported binary value!`. If you need support for binary data, please load the Base64 plugin before you use YAML CPP.
 *
 * @returns A new YAML node containing the data specified in `key`
 */
YAML::Node createNode (Key const & key)
{
	if (key.hasMeta ("array"))
	{
		return YAML::Node (YAML::NodeType::Sequence);
	}
	if (key.getBinarySize () == 0)
	{
		return YAML::Node (YAML::NodeType::Null);
	}
	if (key.isBinary ())
	{
		return YAML::Node ("Unsupported binary value!");
	}

	auto value = key.get<string> ();
	if (value == "0" || value == "1")
	{
		return YAML::Node (key.get<bool> ());
	}
	return YAML::Node (value);
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
	YAML::Node dataNode = createNode (key);

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
	ELEKTRA_LOG_DEBUG ("Add %llu empty array elements", numberOfElements);
	for (auto missingFields = numberOfElements; missingFields > 0; missingFields--)
	{
		sequence.push_back ({});
	}
}

/**
 * @brief This function adds a key that is not part of any array to a YAML node.
 *
 * @param data This node stores the data specified via `keyIterator`.
 * @param keyIterator This iterator specifies the current part of the key name this function adds to `data`.
 * @param key This parameter specifies the key that should be added to `data`.
 */
void addKeyNoArray (YAML::Node & data, NameIterator & keyIterator, Key & key)
{
	if (keyIterator == key.end ())
	{
		ELEKTRA_LOG_DEBUG ("Create leaf node for value “%s”", key.getString ().c_str ());
		data = createLeafNode (key);
		return;
	}

	ELEKTRA_LOG_DEBUG ("Add key part “%s”", (*keyIterator).c_str ());

	string part = *keyIterator;
	if (data.IsScalar ()) data = YAML::Node ();
	YAML::Node node = (data[part] && !data[part].IsScalar ()) ? data[part] : YAML::Node ();
	addKeyNoArray (node, ++keyIterator, key);
	data[part] = node;
}

/**
 * @brief This function adds a key that is either, element of an array, or an array parent to a YAML node.
 *
 * @param data This node stores the data specified via `keyIterator`.
 * @param keyIterator This iterator specifies the current part of the key name this function adds to `data`.
 * @param key This parameter specifies the key that should be added to `data`.
 * @param converted This partial key specifies the part of `key` that is already part of `data`.
 * @param arrayParent This key stores the (possible) array parent of the current part of `key` this
 *                    function should add to `data`.
 */
void addKeyArray (YAML::Node & data, NameIterator & keyIterator, Key & key, Key & converted, Key * arrayParent)
{
	converted.addBaseName (*keyIterator);
	auto const isArrayElement = data.IsSequence () || (arrayParent && converted.isDirectBelow (*arrayParent));
	auto const arrayIndex = isArrayElement ? getArrayIndex (keyIterator) : 0;

	if (keyIterator == key.end ())
	{
		ELEKTRA_LOG_DEBUG ("Create leaf node for value “%s”", key.getString ().c_str ());
		data = createLeafNode (key);
		return;
	}

	ELEKTRA_LOG_DEBUG ("Add key part “%s”", (*keyIterator).c_str ());

	YAML::Node node;

	string part = *keyIterator;
	if (data.IsScalar ()) data = YAML::Node ();
	if (isArrayElement)
	{
		node = (arrayIndex < data.size () && !data[arrayIndex].IsScalar ()) ? data[arrayIndex] : YAML::Node ();
	}
	else
	{
		node = (data[part] && !data[part].IsScalar ()) ? data[part] : YAML::Node ();
	}
	addKeyArray (node, ++keyIterator, key, converted, arrayParent);

	if (isArrayElement)
	{
		if (arrayIndex > data.size ()) addEmptyArrayElements (data, arrayIndex - data.size ());
		data[arrayIndex] = node;
	}
	else
	{
		data[part] = node;
	}
}

/**
 * @brief This function adds a key set to a YAML node.
 *
 * @param data This node stores the data specified via `mappings`.
 * @param mappings This keyset specifies all keys and values this function adds to `data`.
 * @param parent This key is the root of all keys stored in `mappings`.
 * @param isArray This value specifies if the keys inside `keys` are all part of an array (either element or parent), or if none of them is
 *                part of an array.
 */
void addKeys (YAML::Node & data, KeySet const & mappings, Key const & parent, bool const isArray = false)
{
	stack<Key> arrayParents;

	for (auto key : mappings)
	{
		ELEKTRA_LOG_DEBUG ("Convert key “%s”: “%s”", key.getName ().c_str (),
				   key.getBinarySize () == 0 ? "NULL" : key.isString () ? key.getString ().c_str () : "binary value!");
		NameIterator keyIterator = relativeKeyIterator (key, parent);

		if (isArray)
		{
			if (key.hasMeta ("array"))
			{
				ELEKTRA_LOG_DEBUG ("Add array parent “%s”", key.getName ().c_str ());
				arrayParents.push (key);
			}
			else if (!arrayParents.empty () && !key.isBelow (arrayParents.top ()))
			{
				ELEKTRA_LOG_DEBUG ("Remove array parent “%s”", arrayParents.top ().getName ().c_str ());
				arrayParents.pop ();
			}

			Key converted{ parent.getName (), KEY_END };
			addKeyArray (data, keyIterator, key, converted, arrayParents.empty () ? nullptr : &arrayParents.top ());
		}
		else
		{
			addKeyNoArray (data, keyIterator, key);
		}

#ifdef HAVE_LOGGER
		ostringstream output;
		output << data;

		ELEKTRA_LOG_DEBUG ("Converted Data:");
		ELEKTRA_LOG_DEBUG ("——————————");

		istringstream stream (output.str ());
		for (string line; std::getline (stream, line);)
		{
			ELEKTRA_LOG_DEBUG ("%s", line.c_str ());
		}

		ELEKTRA_LOG_DEBUG ("——————————");
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
	KeySet keys = mappings;
	auto missing = missingKeys (keys, parent);
	keys.append (missing);

	KeySet arrayParents;
	KeySet arrays;
	KeySet nonArrays;

	arrayParents = splitArrayParents (keys);
	tie (arrays, nonArrays) = splitArrayOther (arrayParents, keys);

	YAML::Node data;
	addKeys (data, nonArrays, parent);
	addKeys (data, arrays, parent, true);

#ifdef HAVE_LOGGER
	ELEKTRA_LOG_DEBUG ("Write Data:");
	ELEKTRA_LOG_DEBUG ("——————————");

	ostringstream outputString;
	outputString << data;
	istringstream stream (outputString.str ());
	for (string line; std::getline (stream, line);)
	{
		ELEKTRA_LOG_DEBUG ("%s", line.c_str ());
	}

	ELEKTRA_LOG_DEBUG ("——————————");
#endif

	ofstream output (parent.getString ());
	output << data << endl;
}
