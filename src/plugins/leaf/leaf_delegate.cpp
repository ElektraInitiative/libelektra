/**
 * @file
 *
 * @brief Delegate implementation for the `leaf` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "leaf_delegate.hpp"

using std::make_pair;
using std::pair;

// -- Macros -------------------------------------------------------------------------------------------------------------------------------

#define DIRECTORY_POSTFIX "___dirdata"

namespace elektra
{
using CppKey = kdb::Key;

// ===========
// = Private =
// ===========

/**
 * @brief This method copies directory leaves (marked with `DIRECTORY_POSTFIX`) from `input` to the returned key set.
 *
 * @param input The function searches for directory leaves in this key set.

 * @return A key set containing all directory leaves from `input`
 */
CppKeySet LeafDelegate::getDirectoryLeaves (CppKeySet const & input)
{
	CppKeySet directoryLeaves;

	for (auto key : input)
	{
		if (key.getBaseName () == DIRECTORY_POSTFIX)
		{
			directoryLeaves.append (key);
		}
	}

	return directoryLeaves;
}

/**
 * @brief This method removes the directory prefix (`DIRECTORY_POSTFIX`) from the name of all keys in `directoryLeaves`.
 *
 * @pre All key names in `directoryLeaves` must end with `DIRECTORY_POSTFIX`.
 *
 * @param directoryLeaves This parameter contains the keys from which this function removes `DIRECTORY_POSTFIX`
 */
void LeafDelegate::convertLeavesToDirectories (CppKeySet & directoryLeaves)
{
	for (auto key : directoryLeaves)
	{
		keySetBaseName (*key, 0);
	}
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
	for (CppKey previous = nullptr; keys.next (); previous = keys.current ())
	{
		if (keys.current ()->isBelow (previous))
		{
			leaves.append (keys.current ());
		}
		else
		{
			directories.append (keys.current ());
		}
	}
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
		CppKey leaf = directory;
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
 * @brief This method converts all directories keys in the given key set to leaf keys.
 */
void LeafDelegate::convertToLeaves (CppKeySet & keys)
{
	auto directoriesLeaves = splitDirectoriesLeaves (keys);
	keys.clear ();
	keys.append (directoriesLeaves.first);
	keys.append (directoriesLeaves.second);
}

} // end namespace elektra
