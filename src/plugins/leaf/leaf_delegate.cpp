/**
 * @file
 *
 * @brief Delegate implementation for the `leaf` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <numeric>

#include <kdbassert.h>
#include <kdbease.h>
#include <kdblogger.h>

#include "leaf_delegate.hpp"

using std::accumulate;
using std::ignore;
using std::make_pair;
using std::pair;
using std::range_error;
using std::string;
using std::tie;

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

namespace elektra
{
using CppKey = kdb::Key;

/**
 * @brief This function splits the given keyset into directory leaves (marked with `DIRECTORY_POSTFIX`) and other keys.
 *
 * @param input The function searches for directory leaves in this key set.
 *
 * @return A pair of key sets, where the first key set contains all directory leaves and the second key set contains all other keys
 */
pair<CppKeySet, CppKeySet> splitDirectoryLeavesOther (CppKeySet const & input)
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
 * @brief This function removes the directory postfix (`DIRECTORY_POSTFIX`) from the name of all keys in `directoryLeaves`.
 *
 * @pre All key names in `directoryLeaves` must end with `DIRECTORY_POSTFIX`.
 *
 * @param directoryLeaves This parameter contains the keys for which this function removes the directory postfix.
 *
 * @return A copy of the input, where each key name does not end with the directory postfix any more
 */
CppKeySet convertLeavesToDirectories (CppKeySet const & directoryLeaves)
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
 * @brief This function returns a modified copy of `child`, where child is directly below `parent`.
 *
 * For example, if `child` has the name `user/parent/level1/level2/level3` and parent has the name `user/parent`, then the function will
 * return a key with the name `user/parent/level1`.
 *
 * @pre The key `child` has to be below `parent`.
 *
 * @param parent This parameter specifies a parent key of `child`.
 * @param keys This variable stores a child key of `parent`.
 *
 * @return A copy of `child` that is directly below `parent`
 */
CppKey convertToDirectChild (CppKey const & parent, CppKey const & child)
{
	ELEKTRA_ASSERT (child.isBelow (parent), "The key `child` is not located below `parent`");

	CppKey directChild = child.dup ();
	while (!directChild.isDirectBelow (parent))
	{
		keySetBaseName (*directChild, 0);
	}
	return directChild;
}

/**
 * @brief This function determines if the given key is an array parent.
 *
 * @param parent This parameter specifies a possible array parent.
 * @param keys This variable stores the key set of `parent`.
 *
 * @retval true If `parent` is the parent key of an array
 * @retval false Otherwise
 */
bool isArrayParent (CppKey const & parent, CppKeySet const & keys)
{
	CppKeySet children = accumulate (keys.begin (), keys.end (), CppKeySet{}, [&parent](CppKeySet collected, CppKey key) {
		if (key.isBelow (parent)) collected.append (key);
		return collected;
	});

	for (auto child : children)
	{
		CppKey directChild = convertToDirectChild (parent, child);
		if (elektraArrayValidateName (*directChild) < 0) return false;
	}

	return true;
}

/**
 * @brief Split `keys` into two key sets, one for array parents and one for all other keys.
 *
 * @param keys This parameter contains the key set this function splits.
 *
 * @return A pair of key sets, where the first key set contains all array parents and the second key set contains all other keys
 */
pair<CppKeySet, CppKeySet> splitArrayParentsOther (CppKeySet const & keys)
{
	CppKeySet arrayParents;
	CppKeySet others;

	keys.rewind ();
	CppKey previous;
	for (previous = keys.next (); keys.next (); previous = keys.current ())
	{
		bool previousIsArray =
			previous.hasMeta ("array") ||
			(keys.current ().isBelow (previous) && keys.current ().getBaseName ()[0] == '#' && isArrayParent (previous, keys));

		(previousIsArray ? arrayParents : others).append (previous);
	}
	(previous.hasMeta ("array") ? arrayParents : others).append (previous);

	ELEKTRA_ASSERT (arrayParents.size () + others.size () == keys.size (),
			"Number of keys in split key sets: %zu ≠ number in given key set %zu", arrayParents.size () + others.size (),
			keys.size ());

	return make_pair (arrayParents, others);
}

/**
 * @brief This function splits `keys` into two key sets, one for array parents and elements, and the other one for all other keys.
 *
 * @param arrayParents This key set contains a (copy) of all array parents of `keys`.
 * @param keys This parameter contains the key set this function splits.
 *
 * @return A pair of key sets, where the first key set contains all array parents and elements,
 *         and the second key set contains all other keys
 */
pair<CppKeySet, CppKeySet> splitArrayOther (CppKeySet const & arrayParents, CppKeySet const & keys)
{
	CppKeySet others = keys.dup ();
	CppKeySet arrays;

	for (auto parent : arrayParents)
	{
		arrays.append (others.cut (parent));
	}

	return make_pair (arrays, others);
}

/**
 * @brief This function increases an array index of the given array element by one.
 *
 * @param parent This key set stores an array parent of `element`. The function will increase the index of `element` that is directly below
 *               this key.
 * @param element This parameter stores an array element.
 *
 * @return A copy of `element`, where the index below `parent` was increased by one
 */
CppKey increaseArrayIndex (CppKey const & parent, CppKey const & element)
{
	CppKey elementNewIndex = convertToDirectChild (parent, element);
	string postfix = elektraKeyGetRelativeName (*element, *elementNewIndex);

	if (elektraArrayIncName (*elementNewIndex))
	{
		throw range_error ("Unable to increase index of key “" + elementNewIndex.getName () + "”");
	}
	elementNewIndex.addName (postfix);

	ELEKTRA_LOG_DEBUG ("New name of “%s” is “%s”", element.getName ().c_str (), elementNewIndex.getName ().c_str ());

	return elementNewIndex;
}

/**
 * @brief Increase the array index of array elements in `arrays` by one.
 *
 * @param parents This parameter contains the array parents for which this function increases the index by one.
 * @param parents This variable stores the arrays elements this function modifies.
 *
 * @return A copy of `arrays`, where all indices specified by `parents` are increased by one.
 */
CppKeySet increaseArrayIndices (CppKeySet const & parents, CppKeySet const & arrays)
{
	CppKeySet arraysIncreasedIndex = arrays.dup ();
	CppKeySet arrayParents = parents.dup ();

	while (CppKey parent = arrayParents.pop ())
	{
		ELEKTRA_LOG_DEBUG ("Increase indices for array parent “%s”", parent.getName ().c_str ());

		arraysIncreasedIndex = accumulate (arraysIncreasedIndex.begin (), arraysIncreasedIndex.end (), CppKeySet{},
						   [&parent](CppKeySet keys, CppKey key) {
							   keys.append (key.isBelow (parent) ? increaseArrayIndex (parent, key) : key);
							   return keys;
						   });
	}

	return arraysIncreasedIndex;
}

/**
 * @brief Split `keys` into two key sets, one for directories (keys without children) and one for all other keys.
 *
 * @param keys This parameter contains the key set this function splits.
 *
 * @return A pair of key sets, where the first key set contains all directories and the second key set contains all leaves
 */
pair<CppKeySet, CppKeySet> splitDirectoriesLeaves (CppKeySet const & keys)
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
 * @brief Convert all keys in `parents` to an empty array parent and an array element with index `#0` storing the data of the old key.
 *
 * @param parents This parameter contains the set of array parent this function converts.
 *
 * @return A key set containing only empty array parents and corresponding array elements storing the values of the old array parent
 */
CppKeySet convertArrayParentsToLeaves (CppKeySet const & parents)
{
	CppKeySet converted;

	for (auto parent : parents)
	{
		CppKey directory{ parent.getName (), KS_END };
		CppKey leaf = parent.dup ();
		leaf.addBaseName ("#0");
		converted.append (directory);
		converted.append (leaf);
	}

	return converted;
}

/**
 * @brief Convert all keys in `directories` to an empty key and a leaf key containing the data of the old key.
 *
 * @param directories This parameter contains a set of directory keys this function converts.
 *
 * @return A key set containing only empty directory keys and corresponding leaf keys storing the values of the old directory keys
 */
CppKeySet convertDirectoriesToLeaves (CppKeySet const & directories)
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

// -- Class --------------------------------------------------------------------------------------------------------------------------------

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
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS If the plugin converted any value in the given key set
 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE If the plugin did not update `keys`
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
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS If the plugin converted any value in the given key set
 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE If the plugin did not update `keys`
 */
int LeafDelegate::convertToLeaves (CppKeySet & keys)
{
	CppKeySet notArrayParents;
	CppKeySet arrayParents;
	CppKeySet arrays;
	CppKeySet directories;
	CppKeySet leaves;

	tie (arrayParents, notArrayParents) = splitArrayParentsOther (keys);
	tie (arrays, ignore) = splitArrayOther (arrayParents, keys);

	CppKeySet updatedArrays = increaseArrayIndices (arrayParents, arrays);
	CppKeySet updatedArrayParents = convertArrayParentsToLeaves (arrayParents);

	tie (directories, leaves) = splitDirectoriesLeaves (notArrayParents);
	bool status = directories.size () > 0 || arrayParents.size () > 0 ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	auto directoryLeaves = convertDirectoriesToLeaves (directories);

	keys.clear ();
	keys.append (updatedArrays);
	keys.append (updatedArrayParents);
	keys.append (directoryLeaves);
	keys.append (leaves);

	return status;
}

} // end namespace elektra
