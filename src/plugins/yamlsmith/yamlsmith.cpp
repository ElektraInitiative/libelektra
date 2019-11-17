/**
 * @file
 *
 * @brief Source for yamlsmith plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <fstream>
#include <iostream>

#include "yamlsmith.hpp"

#include <kdb.hpp>
#include <kdbease.h>
#include <kdberrors.h>

using std::endl;
using std::ofstream;
using std::string;

using ckdb::Key;
using ckdb::KeySet;

using ckdb::keyNew;

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;
using NameIterator = kdb::NameIterator;

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

namespace
{

/**
 * @brief This function returns a key set containing the contract of the plugin.
 *
 * @return A contract describing the functionality of this plugin
 */
CppKeySet contractYamlsmith ()
{
	return CppKeySet{ 30,
			  keyNew ("system:/elektra/modules/yamlsmith", KEY_VALUE, "yamlsmith plugin waits for your orders", KEY_END),
			  keyNew ("system:/elektra/modules/yamlsmith/exports", KEY_END),
			  keyNew ("system:/elektra/modules/yamlsmith/exports/get", KEY_FUNC, elektraYamlsmithGet, KEY_END),
			  keyNew ("system:/elektra/modules/yamlsmith/exports/set", KEY_FUNC, elektraYamlsmithSet, KEY_END),
#include ELEKTRA_README
			  keyNew ("system:/elektra/modules/yamlsmith/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  keyNew ("system:/elektra/modules/yamlcpp/config/needs/boolean/restore", KEY_VALUE, "#1", KEY_END),
			  KS_END };
}

/**
 * @brief This function collects leaf keys (keys without any key below) for a given key set.
 *
 * @param keys This parameter stores the key set for which this function retrieves all leaf keys.
 *
 * @return A key set containing only leaf keys
 */
CppKeySet leaves (CppKeySet const & keys)
{
	CppKeySet leaves;

	auto current = keys.begin ();
	if (current == keys.end ()) return leaves;

	CppKey previous = *current;
	while (++current != keys.end ())
	{
		bool isLeaf = !current->isBelow (previous);
		if (isLeaf)
		{
			leaves.append (previous);
		}

		previous = *current;
	}
	// The last key is always a leaf
	leaves.append (previous);

	return leaves;
}

/**
 * @brief This function counts the levels of a certain key name.
 *
 * @param key This parameter stores the key for which this function retrieves the number of parts.
 *
 * @return The number of parts of `key`
 */
size_t countKeyLevels (CppKey const & key)
{
	auto keyIterator = key.begin ();
	size_t levels = 0;

	while (keyIterator != key.end ())
	{
		keyIterator++;
		levels++;
	}
	return levels;
}

/**
 * @brief This function writes a YAML collection entry (either mapping key or array element) to the given stream.
 *
 * @pre The parameter `output` must be a valid and open output stream.
 *
 * @param output This parameter specifies where this function should put the serialized YAML data.
 * @param key The function uses the basename of this key to decide if the entry is an array element or a mapping key.
 * @param indent This string specifies the indentation for the collection entry.
 */
inline void writeCollectionEntry (ofstream & output, CppKey const & key, string const & indent)
{
	output << indent;
	if (elektraArrayValidateName (*key) == 1)
	{
		output << "-" << endl;
	}
	else
	{
		output << key.getBaseName () << ":" << endl;
	}
}

/**
 * @brief This function returns a name iterator for a key that starts after `levelsToSkip` levels.
 *
 * @param key This parameter stores the key for which this function returns a name iterator.
 * @param levelsToSkip This value stores the number of parts of `key` that the returned iterator should skip.
 *
 * @return A name iterator for `key`, that skips the first `levelsToSkip` parts of `key`
 */
inline NameIterator getIteratorSkippedLevels (CppKey const & key, size_t levelsToSkip)
{
	auto iterator = key.begin ();
	for (auto levels = levelsToSkip; levels > 0; levels--)
	{
		iterator++;
	}

	return iterator;
}

/**
 * @brief This function writes a representation of a key value to the given output stream.
 *
 * @param output This parameter specifies where this function should emit the serialized YAML data.
 * @param key This parameter stores the key which stores the value this function should emit to `output`.
 */
void writeYAMLScalar (ofstream & output, CppKey const & key)
{
	if (!key.isString ()) return;

	string value = key.getString ();

	if (value == "0")
	{
		output << "false";
		return;
	}

	if (value == "1")
	{
		output << "true";
		return;
	}

	output << '"' << value << '"';
}

/**
 * @brief This function converts a `KeySet` into the YAML serialization format.
 *
 * @pre The parameter `output` must be a valid and open output stream.
 *
 * @param output This parameter specifies where this function should emit the serialized YAML data.
 * @param keys This parameter stores the key set which this function converts to YAML data.
 * @param parent This value represents the root key of `keys`.
 */
void writeYAML (ofstream & output, CppKeySet && keys, CppKey const & parent)
{
	auto levelsParent = countKeyLevels (parent);

	ELEKTRA_LOG_DEBUG ("Convert %zu key%s", keys.size (), keys.size () == 1 ? "" : "s");
	keys.rewind ();
	for (CppKey last = parent; keys.next (); last = keys.current ())
	{
		ELEKTRA_LOG_DEBUG ("Convert key “%s: %s”", keys.current ().getName ().c_str (), keys.current ().getString ().c_str ());

		// Skip common prefix (parent key name) for all keys in key set
		auto relativeLast = getIteratorSkippedLevels (last, levelsParent);
		auto relative = getIteratorSkippedLevels (keys.current (), levelsParent);

		// Add indentation for each part of the key that was already added to the file
		string indent;
		while (relativeLast != last.end () && relative != keys.current ().end () && *relative == *relativeLast)
		{
			relative++;
			relativeLast++;
			indent += "  ";
		}

		// Add YAML mapping key for each part of the key we did not already write into the file
		auto endCurrent = keys.current ().end ();
		CppKey current{ "user:/", KEY_END };

		while (relative != endCurrent)
		{
			current.addBaseName (*relative);
			ELEKTRA_LOG_DEBUG ("Current name: %s", current.getName ().c_str ());
			writeCollectionEntry (output, *current, indent);
			relative++;
			indent += "  ";
		}

		output << indent;
		writeYAMLScalar (output, keys.current ());
		output << endl;
	}
}

} // end namespace

extern "C" {
// ====================
// = Plugin Interface =
// ====================

/** @see elektraDocGet */
int elektraYamlsmithGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	CppKey parent{ parentKey };
	CppKeySet keys{ returned };

	if (parent.getName () == "system:/elektra/modules/yamlsmith")
	{
		keys.append (contractYamlsmith ());
		parent.release ();
		keys.release ();

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	parent.release ();

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

/** @see elektraDocSet */
int elektraYamlsmithSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	CppKey parent{ parentKey };
	CppKeySet keys{ returned };

	ofstream file{ parent.getString () };
	if (file.is_open ())
	{
		writeYAML (file, leaves (keys), parent);
	}
	else
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parent.getKey (), "Unable to open file '%s'", parent.getString ().c_str ());
	}

	parent.release ();
	keys.release ();

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("yamlsmith", ELEKTRA_PLUGIN_GET, &elektraYamlsmithGet, ELEKTRA_PLUGIN_SET, &elektraYamlsmithSet,
				    ELEKTRA_PLUGIN_END);
}

} // end extern "C"
