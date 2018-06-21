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
	return CppKeySet (30, keyNew ("system/elektra/modules/yamlsmith", KEY_VALUE, "yamlsmith plugin waits for your orders", KEY_END),
			  keyNew ("system/elektra/modules/yamlsmith/exports", KEY_END),
			  keyNew ("system/elektra/modules/yamlsmith/exports/get", KEY_FUNC, elektraYamlsmithGet, KEY_END),
			  keyNew ("system/elektra/modules/yamlsmith/exports/set", KEY_FUNC, elektraYamlsmithSet, KEY_END),
#include ELEKTRA_README (yamlsmith)
			  keyNew ("system/elektra/modules/yamlsmith/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
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
NameIterator relativeKeyIterator (CppKey const & key, CppKey const & parent)
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
 * @brief This function compares the structure of two key names.
 *
 * @param key1 The function returns `true` if this `Key` is below or at the same level as `key2`.
 * @param key2 The function returns `true` if this `Key` is above or at the same level as `key1`.
 *
 * @retval true If `key1` is below or on the same level as `key2`
 * @retval false Otherwise
 */
bool sameLevelOrBelow (CppKey const & key1, CppKey const & key2)
{
	if (!key1 || !key2) return false;

	return key2.isBelow (key1) || key1.getFullName ().substr (0, key1.getFullNameSize () - key1.getBaseNameSize ()) ==
					      key2.getFullName ().substr (0, key2.getFullNameSize () - key2.getBaseNameSize ());
}

/**
 * @brief This class provides additional functionality for the key set class.
 */
class CppKeySetPlus : public CppKeySet
{
public:
	/**
	 * @copydoc KeySet::KeySet(ckdb::KeySet)
	 */
	CppKeySetPlus (ckdb::KeySet * keys) : CppKeySet (keys)
	{
	}

	/**
	 * @brief Collect leaf keys (keys without any key below) for this key set.
	 *
	 * @return A key set containing all leaf keys
	 */
	CppKeySet leaves ()
	{
		CppKeySet leaves;

		auto current = this->begin ();
		if (current == this->end ()) return leaves;

		CppKey previous = *current;
		while (++current != this->end ())
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
};

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
 * @brief This function converts a `KeySet` into the YAML serialization format.
 *
 * @pre The parameter `output` must be a valid and open output stream.
 *
 * @param output This parameter specifies where this function should emit the serialized YAML data.
 * @param keys This parameter stores the key set which this function converts to YAML data.
 * @param parent This value represents the root key of `keys`.
 */
void writeYAML (ofstream & output, CppKeySet & keys, CppKey const & parent)
{
	keys.rewind ();
	for (CppKey last = nullptr; keys.next (); last = keys.current ())
	{
		string indent;
		bool sameOrBelowLast = sameLevelOrBelow (last, keys.current ());
		auto relative = relativeKeyIterator (keys.current (), parent);
		auto baseName = keys.current ().rbegin ();
		CppKey current{ keys.current ().getName (), KEY_END };

		while (*relative != *baseName)
		{
			current.addBaseName (*relative);
			ELEKTRA_LOG_DEBUG ("Current name: %s", current.getName ().c_str ());
			if (!sameOrBelowLast) writeCollectionEntry (output, *current, indent);
			relative++;
			indent += "  ";
		}
		writeCollectionEntry (output, *keys.current (), indent);
		if (keys.current ().getStringSize () > 1) output << indent << "  " << keys.current ().getString () << endl;
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

	if (parent.getName () == "system/elektra/modules/yamlsmith")
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
	CppKeySetPlus keys{ returned };

	ofstream file{ parent.getString () };
	if (file.is_open ())
	{
		writeYAML (file, keys, parent);
	}
	else
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_COULD_NOT_OPEN, parent.getKey (), "Unable to open file “%s”",
				    parent.getString ().c_str ());
	}

	parent.release ();
	keys.release ();

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yamlsmith)
{
	return elektraPluginExport ("yamlsmith", ELEKTRA_PLUGIN_GET, &elektraYamlsmithGet, ELEKTRA_PLUGIN_SET, &elektraYamlsmithSet,
				    ELEKTRA_PLUGIN_END);
}

} // end extern "C"
