/**
 * @file
 *
 * @brief Delegate implementation for the `leaf` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdblogger.h>

#include "leaf_delegate.hpp"

using std::make_pair;
using std::pair;
using std::tie;

// -- Macros -------------------------------------------------------------------------------------------------------------------------------

namespace elektra
{
using CppKey = kdb::Key;

// ===========
// = Private =
// ===========

/**
 * @brief This method splits the given keyset into directory leaves (marked with `DIRECTORY_POSTFIX`) and other keys.
 *
 * @param input The function searches for directory leaves in this key set.
 *
 * @return A pair of key sets, where the first key set contains all directory leaves and the second key set contains all other keys
 */
pair<CppKeySet, CppKeySet> LeafDelegate::splitDirectoryLeavesOther (CppKeySet const & input)
{
	CppKeySet directoryLeaves;
	CppKeySet other;

	for (auto key : input)
	{
		if (key.getBaseName () == DIRECTORY_POSTFIX)
		{
			directoryLeaves.append (key);
		}
		else
		{
			other.append (key);
		}
	}
	return make_pair (directoryLeaves, other);
}

/**
 * @brief This method removes the directory postfix (`DIRECTORY_POSTFIX`) from the name of all keys in `directoryLeaves`.
 *
 * @pre All key names in `directoryLeaves` must end with `DIRECTORY_POSTFIX`.
 *
 * @param directoryLeaves This parameter contains the keys for which this function removes the directory postfix.
 *
 * @return A copy of the input, where each key name does not end with the directory postfix any more
 */
CppKeySet LeafDelegate::convertLeavesToDirectories (CppKeySet const & directoryLeaves)
{
	CppKeySet directories;

	for (auto key : directoryLeaves)
	{
		CppKey directory = key.dup ();
		keySetBaseName (*directory, 0);
		directories.append (directory);
	}
	return directories;
}

/**
 * @brief Split `keys` into two key sets, one for directories (keys without children) and one for all other keys.
 *
 * @param keys This parameter contains the key set this function splits.
 *
 * @return A pair of key sets, where the firs key set contains all directories and the second key set contains all leaves
 */
pair<CppKeySet, CppKeySet> LeafDelegate::splitDirectoriesLeaves (CppKeySet const & keys)
{
	CppKeySet leaves;
	CppKeySet directories;

	keys.rewind ();
	CppKey previous;
	for (previous = keys.next (); keys.next (); previous = keys.current ())
	{
		if (keys.current ().isBelow (previous))
		{
			directories.append (previous);
		}
		else
		{
			leaves.append (previous);
		}
	}
	leaves.append (previous);

	return make_pair (directories, leaves);
}

/**
 * @brief Convert all keys in `directories` to an empty key and a leaf key containing the data of the old key.
 *
 * @param directories This parameter contains a set of directory keys this function converts.
 *
 * @return A key set containing only empty directory keys and corresponding leaf keys storing the values of the old directory keys
 */
CppKeySet LeafDelegate::convertDirectoriesToLeaves (CppKeySet const & directories)
{
	CppKeySet directoryLeaves;

	for (auto directory : directories)
	{
		CppKey emptyDirectory{ directory.getName (), KS_END };
		CppKey leaf = directory.dup ();
		leaf.addBaseName (DIRECTORY_POSTFIX);
		directoryLeaves.append (leaf);
		directoryLeaves.append (emptyDirectory);
	}

	return directoryLeaves;
}

// ==========
// = Public =
// ==========

/**
 * @brief This constructor creates a new delegate object used by the `leaf` plugin
 *
 * @param config This key set contains configuration values provided by the `leaf` plugin
 */
LeafDelegate::LeafDelegate (CppKeySet config ELEKTRA_UNUSED)
{
}

/**
 * @brief This method converts all leaf keys in the given key set to directory keys.
 *
 * @param keys This parameter specifies the key set this function converts.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the plugin converted any value in the given key set
 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE if the plugin did not update `keys`
 */
int LeafDelegate::convertToDirectories (CppKeySet & keys)
{
	CppKeySet directoryLeaves;
	CppKeySet nonDirectoryLeaves;
	tie (directoryLeaves, nonDirectoryLeaves) = splitDirectoryLeavesOther (keys);

	bool status = directoryLeaves.size () > 0 ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	auto directories = convertLeavesToDirectories (directoryLeaves);

	keys.clear ();
	keys.append (nonDirectoryLeaves);
	keys.append (directories);

	return status;
}

/**
 * @brief This method converts all directory keys in the given key set to leaf keys.
 *
 * @param keys This parameter specifies the key set this function converts.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the plugin converted any value in the given key set
 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE if the plugin did not update `keys`
 */
int LeafDelegate::convertToLeaves (CppKeySet & keys)
{
	CppKeySet directories;
	CppKeySet leaves;
	tie (directories, leaves) = splitDirectoriesLeaves (keys);
	bool status = directories.size () > 0 ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	auto directoryLeaves = convertDirectoriesToLeaves (directories);

	keys.clear ();
	keys.append (directoryLeaves);
	keys.append (leaves);

	return status;
}

} // end namespace elektra
