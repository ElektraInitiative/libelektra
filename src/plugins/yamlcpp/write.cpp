/**
 * @file
 *
 * @brief Write key sets using yaml-cpp
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "write.hpp"
#include "yaml-cpp/yaml.h"

#include <elektra/ease/old_ease.h>
#include <internal/utility/logger.h>
#include <kdb.hpp>

#include <fstream>
#include <stack>

namespace
{

using std::endl;
using std::istringstream;
using std::ofstream;
using std::ostringstream;
using std::stack;
using std::string;

using YAML::Node;
using YAML::NodeType;

using kdb::Key;
using kdb::KeySet;
using kdb::NameIterator;

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
uintmax_t getArrayIndex (NameIterator const & nameIterator)
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
Node createDataNode (Key const & key)
{
	if (key.hasMeta ("array"))
	{
		return Node (NodeType::Sequence);
	}
	if (key.getBinarySize () == 0)
	{
		return Node (NodeType::Null);
	}
	if (key.isBinary ())
	{
		return Node ("Unsupported binary value!");
	}

	auto value = key.get<string> ();
	if (key.getMeta<string> ("type") == "boolean")
	{
		return Node (key.get<bool> ());
	}

	Node node{ value };
	if (key.getMeta<string> ("type") == "binary") node.SetTag ("tag:yaml.org,2002:binary");

	return node;
}

/**
 * @brief This function creates a YAML Node representing metadata.
 *
 * @param key This key specifies the metadata that should be saved in the YAML node returned by this function.
 *
 * @returns A new YAML node containing the metadata specified in `key`
 */
Node createMetaNode (Key & key)
{
	Node metaNode{ NodeType::Map };

	ckdb::KeySet * metaKeys = ckdb::keyMeta (key.getKey ());

	for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys); ++it)
	{
		const kdb::Key curMeta = ckdb::ksAtCursor (metaKeys, it);
		if (curMeta.getName () == "meta:/array" || curMeta.getName () == "meta:/binary" ||
		    (curMeta.getName () == "meta:/type" && (curMeta.getString () == "boolean" || curMeta.getString () == "binary")))
		{
			continue;
		}
		metaNode[curMeta.getName ().substr (sizeof ("meta:/") - 1)] = curMeta.getString ();
		ELEKTRA_LOG_DEBUG ("Add metakey “%s: %s”", curMeta.getName ().c_str (), curMeta.getString ().c_str ());
	}

	return metaNode;
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
Node createLeafNode (Key & key)
{
	Node dataNode = createDataNode (key);
	Node metaNode = createMetaNode (key);

	if (metaNode.size () <= 0)
	{
		ELEKTRA_LOG_DEBUG ("Return leaf node with value “%s”", dataNode.IsNull ()     ? "~" :
								       dataNode.IsSequence () ? "[]" :
												dataNode.as<string> ().c_str ());
		return dataNode;
	}

	Node node{ NodeType::Sequence };
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
 * @brief This function adds `null` elements to the given YAML sequence.
 *
 * @param sequence This node stores the collection to which this function adds `numberOfElements` empty elements.
 * @param numberOfElements This parameter specifies the number of empty element this function adds to `sequence`.
 */
void addEmptyArrayElements (Node & sequence, uintmax_t const numberOfElements)
{
	ELEKTRA_LOG_DEBUG ("Add %ju empty array elements", numberOfElements);
	for (auto missingFields = numberOfElements; missingFields > 0; missingFields--)
	{
		sequence.push_back ({});
	}
}

/**
 * @brief This function adds a key to a YAML node.
 *
 * @param data This node stores the data specified via the other parameters of this function.
 * @param keyIterator This iterator specifies the current part of the key name this function adds to `data`.
 * @param key This parameter specifies the key that should be added to `data`.
 * @param converted This partial key specifies the part of `key` that is already part of `data`.
 * @param arrayParent This key stores the array parent of the current part of `key` this function should add to `data`. This parameter must
 *                    only contain a valid array parent for the first element key of an array.
 */
void addKey (Node & data, NameIterator & keyIterator, Key & key, Key & converted, Key * arrayParent)
{
	if (keyIterator == key.end ())
	{
		ELEKTRA_LOG_DEBUG ("Create leaf node for value “%s”", key.getString ().c_str ());
		data = createLeafNode (key);
		return;
	}

	converted.addBaseName (*keyIterator);
	auto const isArrayElement = data.IsSequence () || (arrayParent && converted.isDirectBelow (*arrayParent));
	auto const arrayIndex = isArrayElement ? getArrayIndex (keyIterator) : 0;

	ELEKTRA_LOG_DEBUG ("Add key part “%s”", (*keyIterator).c_str ());

	if (data.IsScalar ()) data = Node ();
	if (isArrayElement)
	{
		Node node = arrayIndex < data.size () ? data[arrayIndex] : Node ();
		addKey (node, ++keyIterator, key, converted, arrayParent);
		if (arrayIndex > data.size ()) addEmptyArrayElements (data, arrayIndex - data.size ());
		data[arrayIndex] = node;
	}
	else
	{
		string part = *keyIterator;
		Node node = data[part] ? data[part] : Node ();
		addKey (node, ++keyIterator, key, converted, arrayParent);
		data[part] = node;
	}
}

/**
 * @brief This function adds a key set to a YAML node.
 *
 * @param data This node stores the data specified via the other parameters of this function.
 * @param mappings This keyset specifies all keys and values this function adds to `data`.
 * @param parent This key is the root of all keys stored in `mappings`.
 */
void addKeys (Node & data, KeySet const & mappings, Key const & parent)
{
	/* This stack stores the current array parents for a certain part of a key. The code below only guarantees that the
	   array parents will be correct for the first array element below a parent. */
	stack<Key> arrayParents;

	for (auto key : mappings)
	{
		ELEKTRA_LOG_DEBUG ("Convert key “%s”: “%s”", key.getName ().c_str (),
				   key.getBinarySize () == 0 ? "NULL" :
				   key.isString ()	     ? key.getString ().c_str () :
							       "binary value!");

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

		NameIterator keyIterator = relativeKeyIterator (key, parent);
		Key converted{ parent.getName (), KEY_END };
		addKey (data, keyIterator, key, converted, arrayParents.empty () ? nullptr : &arrayParents.top ());

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
	Node data;
	addKeys (data, keys, parent);

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
