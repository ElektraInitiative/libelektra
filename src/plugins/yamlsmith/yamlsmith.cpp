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

using ckdb::elektraKeyGetRelativeName;
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
		for (auto key : keys.leaves ())
		{
			string indent;
			auto relative = relativeKeyIterator (*key, parent);
			while (relative != key.end ())
			{
				file << indent << *relative << ":" << endl;
				relative++;
				indent += "  ";
			}
			file << indent << key.getString () << endl;
		}
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
